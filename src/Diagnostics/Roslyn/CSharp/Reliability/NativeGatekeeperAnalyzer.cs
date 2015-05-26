// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Roslyn.Diagnostics.Analyzers.CSharp.Reliability
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public sealed class NativeGatekeeperAnalyzer : DiagnosticAnalyzer
    {
        private static readonly LocalizableString s_localizableArrayPointerElementMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeArrayPointerElementMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        private static readonly LocalizableString s_localizableArrayMoreThanFourDimensionsMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeArrayMoreThanFourDimensionsMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        private static readonly LocalizableString s_localizableIEquatableEqualsMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeIEquatableEqualsMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));

        public static readonly DiagnosticDescriptor ArrayPointerElementDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeArrayPointerElementRuleId,
            s_localizableArrayPointerElementMessageAndTitle,
            s_localizableArrayPointerElementMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        public static readonly DiagnosticDescriptor ArrayMoreThanFourDimensionsDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeArrayMoreThanFourDimensionsRuleId,
            s_localizableArrayMoreThanFourDimensionsMessageAndTitle,
            s_localizableArrayMoreThanFourDimensionsMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        public static readonly DiagnosticDescriptor IEquatableEqualsDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeIEquatableEqualsRuleId,
            s_localizableIEquatableEqualsMessageAndTitle,
            s_localizableIEquatableEqualsMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get
            {
                return ImmutableArray.Create(ArrayPointerElementDescriptor, ArrayMoreThanFourDimensionsDescriptor, IEquatableEqualsDescriptor);
            }
        }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSymbolAction(
                (symbolContext) =>
                {
                    AnalyzeMethod(symbolContext, (IMethodSymbol)symbolContext.Symbol);
                },
                SymbolKind.Method);

            context.RegisterSymbolAction(
                (symbolContext) =>
                {
                    AnalyzeProperty(symbolContext, (IPropertySymbol)symbolContext.Symbol);
                },
                SymbolKind.Property);

            context.RegisterSymbolAction(
                (symbolContext) =>
                {
                    AnalyzeField(symbolContext, (IFieldSymbol)symbolContext.Symbol);
                },
                SymbolKind.Field);

            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    AnalyzeArrayCreation(nodeContext, (ArrayCreationExpressionSyntax)nodeContext.Node);
                },
                SyntaxKind.ArrayCreationExpression);

            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    AnalyzeArrayElementReference(nodeContext, (ElementAccessExpressionSyntax)nodeContext.Node);
                },
                SyntaxKind.ElementAccessExpression);

            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    AnalyzeLocalDeclaration(nodeContext, (LocalDeclarationStatementSyntax)nodeContext.Node);
                },
                SyntaxKind.LocalDeclarationStatement);

            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    AnalyzeTypeArguments(nodeContext, (TypeArgumentListSyntax)nodeContext.Node);
                },
                SyntaxKind.TypeArgumentList);

            context.RegisterSyntaxNodeAction(
              (nodeContext) =>
              {
                  AnalyzeArrayTypeSyntax(nodeContext, (ArrayTypeSyntax)nodeContext.Node);
              },
              SyntaxKind.ArrayType);

            context.RegisterCompilationStartAction(OnCompilationStart);
        }

        private void OnCompilationStart(CompilationStartAnalysisContext context)
        {
            INamedTypeSymbol systemObject = context.Compilation.GetTypeByMetadataName("System.Object");
            if (systemObject != null)
            {
                IMethodSymbol objectEquals = null;
                foreach (ISymbol equals in systemObject.GetMembers("Equals"))
                {
                    if (equals.Kind == SymbolKind.Method)
                    {
                        IMethodSymbol equalsMethod = (IMethodSymbol)equals;
                        if (equalsMethod.IsVirtual && equalsMethod.Parameters.Length == 1)
                        {
                            objectEquals = equalsMethod;
                            break;
                        }
                    }
                }

                INamedTypeSymbol iEquatable = context.Compilation.GetTypeByMetadataName("System.IEquatable`1");

                if (iEquatable != null && objectEquals != null)
                {
                    context.RegisterSymbolAction(symbolContext => AnalyzeTypeForIEquatable(symbolContext, (INamedTypeSymbol)symbolContext.Symbol, iEquatable, objectEquals), SymbolKind.NamedType);
                }
            }
        }

#if false
        private void AnalyzeCall(SyntaxNodeAnalysisContext context, ISymbol immutableArrayType)
        {
            var invokeSyntax = context.Node as InvocationExpressionSyntax;
            if (invokeSyntax == null)
            {
                return;
            }

            var memberSyntax = invokeSyntax.Expression as MemberAccessExpressionSyntax;
            if (memberSyntax == null ||
                memberSyntax.Name == null ||
                memberSyntax.Name.Identifier.ValueText != "ToImmutableArray")
            {
                return;
            }

            var targetType = context.SemanticModel.GetTypeInfo(memberSyntax.Expression, context.CancellationToken);
            if (targetType.Type != null && targetType.Type.OriginalDefinition.Equals(immutableArrayType))
            {
                context.ReportDiagnostic(Diagnostic.Create(DoNotCallToImmutableArrayDescriptor, context.Node.GetLocation()));
            }
        }
#endif

        private void AnalyzeTypeForIEquatable(SymbolAnalysisContext context, INamedTypeSymbol namedType, INamedTypeSymbol iEquatable, IMethodSymbol objectEquals)
        {
            if (namedType.TypeKind == TypeKind.Class)
            {
                foreach (INamedTypeSymbol implemented in namedType.Interfaces)
                {
                    if (implemented.OriginalDefinition.Equals(iEquatable))
                    {
                        // Type implements System.IEquatable<T>.
                        foreach (ISymbol member in namedType.GetMembers("Equals"))
                        {
                            if (member.Kind == SymbolKind.Method)
                            {
                                IMethodSymbol overriddenMethod = ((IMethodSymbol)member).OverriddenMethod;
                                if (overriddenMethod != null && overriddenMethod.Equals(objectEquals))
                                {
                                    // Type overrides Object.Equals(object).
                                    return;
                                }
                            }
                        }

                        // Type does not override Object.Equals(object).
                        context.ReportDiagnostic(Diagnostic.Create(IEquatableEqualsDescriptor, namedType.Locations[0]));
                    }
                }
            }
        }

        private void AnalyzeField(SymbolAnalysisContext context, IFieldSymbol field)
        {
           
        }

        private void AnalyzeMethod(SymbolAnalysisContext context, IMethodSymbol method)
        {
            if (method.MethodKind != MethodKind.PropertyGet && method.MethodKind != MethodKind.PropertySet)
            {
                foreach (IParameterSymbol parameter in method.Parameters)
                {
                }
            }
        }

        private void AnalyzeProperty(SymbolAnalysisContext context, IPropertySymbol property)
        {
        }

        private void AnalyzeArrayCreation(SyntaxNodeAnalysisContext context, ArrayCreationExpressionSyntax arrayCreation)
        {
          
        }

        private void AnalyzeArrayElementReference(SyntaxNodeAnalysisContext context, ElementAccessExpressionSyntax elementAccess)
        {
           
        }

        private void AnalyzeLocalDeclaration(SyntaxNodeAnalysisContext context, LocalDeclarationStatementSyntax declaration)
        {
            foreach (VariableDeclaratorSyntax declarator in declaration.Declaration.Variables)
            {
                ILocalSymbol local = (ILocalSymbol)context.SemanticModel.GetDeclaredSymbol(declarator);
            
            }
        }

        private void AnalyzeTypeArguments(SyntaxNodeAnalysisContext context, TypeArgumentListSyntax argumentList)
        {
            foreach (TypeSyntax typeSyntax in argumentList.Arguments)
            {
                ITypeSymbol type = context.SemanticModel.GetTypeInfo(typeSyntax).Type;
               
            }
        }

        private void AnalyzeArrayTypeSyntax(SyntaxNodeAnalysisContext context, ArrayTypeSyntax arrayTypeSyntax)
        {
            foreach (ArrayRankSpecifierSyntax rankSpecifier in arrayTypeSyntax.RankSpecifiers)
            {
                if (rankSpecifier.Rank > 4)
                {
                    context.ReportDiagnostic(Diagnostic.Create(ArrayMoreThanFourDimensionsDescriptor, rankSpecifier.GetLocation()));
                }
            }

            ITypeSymbol elementType = context.SemanticModel.GetTypeInfo(arrayTypeSyntax.ElementType).Type;
            if (elementType.TypeKind == TypeKind.Pointer)
            {
                context.ReportDiagnostic(Diagnostic.Create(ArrayPointerElementDescriptor, arrayTypeSyntax.GetLocation()));
            }
        }
    }
}
