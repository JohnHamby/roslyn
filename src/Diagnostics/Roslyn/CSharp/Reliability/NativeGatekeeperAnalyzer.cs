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

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get
            {
                return ImmutableArray.Create(ArrayPointerElementDescriptor, ArrayMoreThanFourDimensionsDescriptor);
            }
        }

        public override void Initialize(AnalysisContext context)
        {
#if false
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
#endif
            context.RegisterSyntaxNodeAction(
              (nodeContext) =>
              {
                  AnalyzeArrayTypeSyntax(nodeContext, (ArrayTypeSyntax)nodeContext.Node);
              },
              SyntaxKind.ArrayType);

            // context.RegisterCompilationStartAction(OnCompilationStart);
        }

#if false
        private void OnCompilationStart(CompilationStartAnalysisContext context)
        {
            var immutableArrayType = context.Compilation.GetTypeByMetadataName(ImmutableArrayMetadataName);
            if (immutableArrayType != null)
            {
                context.RegisterSyntaxNodeAction(syntaxContext => AnalyzeCall(syntaxContext, immutableArrayType), SyntaxKind.InvocationExpression);
            }
        }

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

        private void AnalyzeField(SymbolAnalysisContext context, IFieldSymbol field)
        {
            AnalyzeArrayType(field.Type, field.Locations, context.ReportDiagnostic);
        }

        private void AnalyzeMethod(SymbolAnalysisContext context, IMethodSymbol method)
        {
            if (method.MethodKind != MethodKind.PropertyGet && method.MethodKind != MethodKind.PropertySet)
            {
                AnalyzeArrayType(method.ReturnType, method.Locations, context.ReportDiagnostic);
                foreach (IParameterSymbol parameter in method.Parameters)
                {
                    AnalyzeArrayType(parameter.Type, parameter.Locations, context.ReportDiagnostic);
                }
            }
        }

        private void AnalyzeProperty(SymbolAnalysisContext context, IPropertySymbol property)
        {
            AnalyzeArrayType(property.Type, property.Locations, context.ReportDiagnostic);
        }

        private void AnalyzeArrayCreation(SyntaxNodeAnalysisContext context, ArrayCreationExpressionSyntax arrayCreation)
        {
            TypeInfo arrayTypeInfo = context.SemanticModel.GetTypeInfo(arrayCreation);
            AnalyzeArrayType(arrayTypeInfo.Type, arrayCreation.GetLocation(), context.ReportDiagnostic);
        }

        private void AnalyzeArrayElementReference(SyntaxNodeAnalysisContext context, ElementAccessExpressionSyntax elementAccess)
        {
            TypeInfo arrayTypeInfo = context.SemanticModel.GetTypeInfo(elementAccess.Expression);
            AnalyzeArrayType(arrayTypeInfo.Type, elementAccess.Expression.GetLocation(), context.ReportDiagnostic);
        }

        private void AnalyzeLocalDeclaration(SyntaxNodeAnalysisContext context, LocalDeclarationStatementSyntax declaration)
        {
            foreach (VariableDeclaratorSyntax declarator in declaration.Declaration.Variables)
            {
                ILocalSymbol local = (ILocalSymbol)context.SemanticModel.GetDeclaredSymbol(declarator);
                AnalyzeArrayType(local.Type, declarator.GetLocation(), context.ReportDiagnostic);
            }
        }

        private void AnalyzeTypeArguments(SyntaxNodeAnalysisContext context, TypeArgumentListSyntax argumentList)
        {
            foreach (TypeSyntax typeSyntax in argumentList.Arguments)
            {
                ITypeSymbol type = context.SemanticModel.GetTypeInfo(typeSyntax).Type;
                AnalyzeArrayType(type, typeSyntax.GetLocation(), context.ReportDiagnostic);
            }
        }

        private void AnalyzeArrayTypeSyntax(SyntaxNodeAnalysisContext context, ArrayTypeSyntax arrayTypeSyntax)
        {
            ITypeSymbol type = context.SemanticModel.GetTypeInfo(arrayTypeSyntax).Type;
            AnalyzeArrayType(type, arrayTypeSyntax.GetLocation(), context.ReportDiagnostic);
        }

        private void AnalyzeArrayType(ITypeSymbol type, ImmutableArray<Location> locations, Action<Diagnostic> reportDiagnostic)
        {
            AnalyzeArrayType(type, locations[0], reportDiagnostic);
        }

        private void AnalyzeArrayType(ITypeSymbol type, Location location, Action<Diagnostic> reportDiagnostic)
        {
            if (type.TypeKind == TypeKind.Array)
            {
                AnalyzeArrayType((IArrayTypeSymbol)type, location, reportDiagnostic);
            }
        }

        private void AnalyzeArrayType(IArrayTypeSymbol arrayType, Location location, Action<Diagnostic> reportDiagnostic)
        {
            if (arrayType.ElementType.TypeKind == TypeKind.Pointer)
            {
                reportDiagnostic(Diagnostic.Create(ArrayPointerElementDescriptor, location));
            }

            if (arrayType.Rank > 4)
            {
                reportDiagnostic(Diagnostic.Create(ArrayMoreThanFourDimensionsDescriptor, location));
            }
        }
    }
}
