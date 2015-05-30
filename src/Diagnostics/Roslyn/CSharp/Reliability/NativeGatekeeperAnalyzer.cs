// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

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
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
        {
            get
            {
                return ImmutableArray.Create(
                    ArrayPointerElementDescriptor,
                    ArrayMoreThanFourDimensionsDescriptor,
                    IEquatableEqualsDescriptor,
                    ClassInterfaceAttributeValueDescriptor);
            }
        }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(
                (nodeContext) =>
                {
                    AnalyzeArrayTypeSyntax(nodeContext, (ArrayTypeSyntax)nodeContext.Node);
                },
                SyntaxKind.ArrayType);

            context.RegisterCompilationStartAction(IEquatableAndEquals);
            context.RegisterCompilationStartAction(ClassInterfaceAttribute);
        }

        private void AnalyzeArrayTypeSyntax(SyntaxNodeAnalysisContext context, ArrayTypeSyntax arrayTypeSyntax)
        {
            // Detect array types with more than four dimensions.
            foreach (ArrayRankSpecifierSyntax rankSpecifier in arrayTypeSyntax.RankSpecifiers)
            {
                if (rankSpecifier.Rank > 4)
                {
                    context.ReportDiagnostic(Diagnostic.Create(ArrayMoreThanFourDimensionsDescriptor, rankSpecifier.GetLocation()));
                }
            }

            // Detect array types with pointer element types.
            ITypeSymbol elementType = context.SemanticModel.GetTypeInfo(arrayTypeSyntax.ElementType).Type;
            if (elementType.TypeKind == TypeKind.Pointer)
            {
                context.ReportDiagnostic(Diagnostic.Create(ArrayPointerElementDescriptor, arrayTypeSyntax.GetLocation()));
            }
        }

        private void IEquatableAndEquals(CompilationStartAnalysisContext context)
        {
            // Find System.Object, System.Object.Equals(object), and System.IEquatable<T>.
            // Register an action for the IEquatable/Equals rule only if all are present.
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

        private void ClassInterfaceAttribute(CompilationStartAnalysisContext context)
        {
            // Find System.Runtime.InteropServices.ClassInterfaceAttribute..ctor(ClassInterfaceType),
            // System.Runtime.InteropServices.ClassInterfaceType.AutoDispatch and System.Runtime.InteropServices.ClassInterfaceType.AutoDual.
            // Register an action for the rule only if all are present.

            INamedTypeSymbol classInterfaceType = context.Compilation.GetTypeByMetadataName("System.Runtime.InteropServices.ClassInterfaceType");
            if (classInterfaceType != null)
            {
                IFieldSymbol autoDispatch = null;
                IFieldSymbol autoDual = null;

                foreach (ISymbol classInterfaceTypeMember in classInterfaceType.GetMembers())
                {
                    if (classInterfaceTypeMember.Kind == SymbolKind.Field)
                    {
                        IFieldSymbol classInterfaceTypeField = (IFieldSymbol)classInterfaceTypeMember;
                        if (classInterfaceTypeField.HasConstantValue && classInterfaceTypeField.Type.Equals(classInterfaceType))
                        {
                            switch (classInterfaceTypeField.Name)
                            {
                                case "AutoDispatch":
                                    autoDispatch = classInterfaceTypeField;
                                    break;
                                case "AutoDual":
                                    autoDual = classInterfaceTypeField;
                                    break;
                            }
                        }
                    }
                }

                INamedTypeSymbol classInterfaceAttribute = context.Compilation.GetTypeByMetadataName("System.Runtime.InteropServices.ClassInterfaceAttribute");
                if (classInterfaceAttribute != null)
                {
                    IMethodSymbol classInterfaceAttributeConstructor1 = null;
                    IMethodSymbol classInterfaceAttributeConstructor2 = null;
                    foreach (IMethodSymbol constructor in classInterfaceAttribute.Constructors)
                    {
                        if (constructor.Parameters.Length == 1)
                        {
                            if (constructor.Parameters[0].Type.Equals(classInterfaceType))
                            {
                                classInterfaceAttributeConstructor1 = constructor;
                            }
                            else if (constructor.Parameters[0].Type.SpecialType == SpecialType.System_Int16)
                            {
                                classInterfaceAttributeConstructor2 = constructor;
                            }
                        }
                    }

                    if (autoDispatch != null && autoDual != null && classInterfaceAttributeConstructor1 != null && classInterfaceAttributeConstructor2 != null)
                    {
                        context.RegisterSymbolAction(symbolContext => AnalyzeForClassInterfaceAttribute(symbolContext, (INamedTypeSymbol)symbolContext.Symbol, classInterfaceAttributeConstructor1, classInterfaceAttributeConstructor2, (int)autoDispatch.ConstantValue, (int)autoDual.ConstantValue), SymbolKind.NamedType);
                    }
                }
            }
        }

        private void AnalyzeForClassInterfaceAttribute(SymbolAnalysisContext context, INamedTypeSymbol namedType, IMethodSymbol classInterfaceAttributeConstructor1, IMethodSymbol classInterfaceAttributeConstructor2, int autoDispatch, int autoDual)
        {
            if (namedType.TypeKind == TypeKind.Class)
            {
                foreach (AttributeData attribute in namedType.GetAttributes())
                {
                    if (attribute.AttributeConstructor != null && (attribute.AttributeConstructor.Equals(classInterfaceAttributeConstructor1) || attribute.AttributeConstructor.Equals(classInterfaceAttributeConstructor2)) && attribute.ConstructorArguments.Length == 1)
                    {
                        TypedConstant argument = attribute.ConstructorArguments[0];
                        if (argument.Kind == TypedConstantKind.Enum || argument.Kind == TypedConstantKind.Primitive)
                        {
                            int value = argument.Value is short ? (short)argument.Value : (int)argument.Value;
                            if (value == autoDispatch || value == autoDual)
                            {
                                // Constructor argument is not ClassInterfaceType.None.
                                context.ReportDiagnostic(Diagnostic.Create(ClassInterfaceAttributeValueDescriptor, attribute.ApplicationSyntaxReference.GetSyntax().GetLocation()));
                            }
                        }
                    }
                }
            }
        }

        private static readonly LocalizableString s_localizableArrayPointerElementMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeArrayPointerElementMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor ArrayPointerElementDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeArrayPointerElementRuleId,
            s_localizableArrayPointerElementMessageAndTitle,
            s_localizableArrayPointerElementMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        private static readonly LocalizableString s_localizableArrayMoreThanFourDimensionsMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeArrayMoreThanFourDimensionsMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor ArrayMoreThanFourDimensionsDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeArrayMoreThanFourDimensionsRuleId,
            s_localizableArrayMoreThanFourDimensionsMessageAndTitle,
            s_localizableArrayMoreThanFourDimensionsMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        private static readonly LocalizableString s_localizableIEquatableEqualsMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeIEquatableEqualsMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor IEquatableEqualsDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeIEquatableEqualsRuleId,
            s_localizableIEquatableEqualsMessageAndTitle,
            s_localizableIEquatableEqualsMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);

        private static readonly LocalizableString s_localizableClassInterfaceAttributeValueMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeClassInterfaceAttributeValueMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor ClassInterfaceAttributeValueDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeClassInterfaceAttributeValueRuleId,
            s_localizableClassInterfaceAttributeValueMessageAndTitle,
            s_localizableClassInterfaceAttributeValueMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Warning,
            isEnabledByDefault: true);
    }
}
