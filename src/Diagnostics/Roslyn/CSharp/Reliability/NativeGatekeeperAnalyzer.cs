﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

// Implements .Net Native Gatekeeper rules applicable to C# source.
//
// a) An array type cannot have a pointer type as an element type.
// b) A type that implements IEquatable<T> must override Object.Equals().
// * c) Creation of a System.Diagnostics.Tracing.EventSourceAttribute instance cannot specify a value for the LocalizationResources property.
// d) Type.GetRuntimeMethods() does not return hidden methods in base types. (This seems like an informational message only.)
// e) Type.GetType(string) searches System.Runtime only.Use Assembly.GetType(string) to search another assembly.
// * f) The body of an infinite loop must do more than store constant values to locals.
// g) An array type cannot have more than 4 dimensions.
// h) The TypeInfo.GUID property will throw PlatformNotSupportedException if the type does not have a Guid attribute applied to it.
// i) Neither ClassInterfaceType.AutoDispatch nor ClassInterfaceType.GetHashCode can be used in creating a System.Runtime.InteropServices.ClassInterfaceAttribute instance. Only ClassInterfaceType.None is allowed.
// * j) Referring to either the BeginInvoke method or the EndInvoke method of a delegate type is prohibited.
// * k) A reference to one of a set of disallowed contract assemblies is prohibited. (The set of unsupported contracts in GatekeeperConfig.xml appears to be empty, so this rule is a placebo at present.)
// * l) A reference to System.Composition.Convention must be to version 1.0.30 or newer.
// * m) Referring to one of a set of disallowed methods is prohibited. (There are a few dozen unsupported methods specified in GatekeeperConfig.xml.)
// * n) Referring to one of a set of disallowed types is prohibited. (There are 14 unsupported types specified in GatekeeperConfig.xml)
// * o) A value type must not exceed 1,000,000 bytes in instance size.
// * p) A class cannot implement more than one interface that has a Windows.Foundation.Metadata.DefaultAttribute attribute.
// * q) A public member of a type defined in a WinMD cannot refer to the types System.IntPtr or System.UIntPtr in a method signature, method return type, or property type. (Gatekeeper also appears to be attempting to prohibit using these types as generic type arguments to public types, but the code looks buggy and I’m not 100% sure of the intent.I have a question out to a Project N developer.)


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
                    ClassInterfaceAttributeValueDescriptor,
                    TypeInfoGUIDDescriptor,
                    TypeGetRuntimeMethodsDescriptor,
                    TypeGetTypeDescriptor);
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
            context.RegisterCompilationStartAction(TypeInfoGUID);
            context.RegisterCompilationStartAction(TypeMethods);
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

        private void TypeInfoGUID(CompilationStartAnalysisContext context)
        {
            // Register an action for the rule only if System.Reflection.TypeInfo.GUID is present.

            INamedTypeSymbol typeInfo = context.Compilation.GetTypeByMetadataName("System.Reflection.TypeInfo");
            if (typeInfo != null)
            {
                foreach (ISymbol guidMember in GetAllMembers(typeInfo, "GUID"))
                {
                    if (guidMember.Kind == SymbolKind.Property)
                    {
                        context.RegisterSyntaxNodeAction(nodeContext => AnalyzeForTypeInfoGUID(nodeContext, typeInfo, (IPropertySymbol)guidMember), SyntaxKind.SimpleMemberAccessExpression, SyntaxKind.ConditionalAccessExpression);
                        return;
                    }
                }
            }
        }

        private void AnalyzeForTypeInfoGUID(SyntaxNodeAnalysisContext context, INamedTypeSymbol typeInfo, IPropertySymbol guid)
        {
            SimpleNameSyntax memberName = null;
            ExpressionSyntax baseReference = null;

            if (TryGetMemberReferenceInfo((ExpressionSyntax)context.Node, out baseReference, out memberName))
            {
                // Check only references that go through TypeInfo, not those that go directly through Type.
                ITypeSymbol baseReferenceType = context.SemanticModel.GetTypeInfo(baseReference).Type;
                if (baseReferenceType != null && typeInfo.Equals(baseReferenceType))
                {
                    CheckForMember(context, memberName, guid, TypeInfoGUIDDescriptor);
                }
            }
        }

        private void TypeMethods(CompilationStartAnalysisContext context)
        {
            // Find System.Type.GetType(string) and System.Reflection.RuntimeReflectionExtensions.GetRuntimeMethods(System.Type), and register an action if they are both present.

            INamedTypeSymbol type = context.Compilation.GetTypeByMetadataName("System.Type");
            if (type != null)
            {
                IMethodSymbol getRuntimeMethods = null;
                IMethodSymbol getType = null;
                
                foreach (ISymbol getTypeMember in type.GetMembers("GetType"))
                {
                    if (getTypeMember.Kind == SymbolKind.Method)
                    {
                        IMethodSymbol method = (IMethodSymbol)getTypeMember;
                        if (method.Parameters.Length == 1 && method.Parameters[0].Type.SpecialType == SpecialType.System_String)
                        {
                            getType = method;
                            break;
                        }
                    }
                }

                INamedTypeSymbol runtimeExtensions = context.Compilation.GetTypeByMetadataName("System.Reflection.RuntimeReflectionExtensions");
                if (runtimeExtensions != null)
                {
                    foreach (ISymbol getRuntimeMethodsMember in runtimeExtensions.GetMembers("GetRuntimeMethods"))
                    {
                        if (getRuntimeMethodsMember.Kind == SymbolKind.Method)
                        {
                            IMethodSymbol method = (IMethodSymbol)getRuntimeMethodsMember;
                            if (method.Parameters.Length == 1 && type.Equals(method.Parameters[0].Type))
                            {
                                getRuntimeMethods = method;
                                break;
                            }
                        }
                    }
                }

                if (getRuntimeMethods != null && getType != null)
                {
                    context.RegisterSyntaxNodeAction(nodeContext => AnalyzeForTypeMethods(nodeContext, getRuntimeMethods, getType), SyntaxKind.SimpleMemberAccessExpression, SyntaxKind.ConditionalAccessExpression);
                }
            }
        }

        private void AnalyzeForTypeMethods(SyntaxNodeAnalysisContext context, IMethodSymbol getRuntimeMethods, IMethodSymbol getType)
        {
            SimpleNameSyntax memberName = null;
            ExpressionSyntax baseReference = null;

            if (TryGetMemberReferenceInfo((ExpressionSyntax)context.Node, out baseReference, out memberName))
            {
                CheckForMember(context, memberName, getType, TypeGetTypeDescriptor);
                CheckForMember(context, memberName, getRuntimeMethods, TypeGetRuntimeMethodsDescriptor);
            }
        }

        private void CheckForMember(SyntaxNodeAnalysisContext context, SimpleNameSyntax memberName, ISymbol member, DiagnosticDescriptor diagnostic)
        {
            if (memberName != null && memberName.Identifier.Text == member.Name)
            {
                ISymbol referencedSymbol = context.SemanticModel.GetSymbolInfo(memberName).Symbol;
                if (referencedSymbol != null)
                {
                    if (referencedSymbol.Kind == SymbolKind.Method)
                    {
                        IMethodSymbol referencedMethod = (IMethodSymbol)referencedSymbol;
                        if (referencedMethod.IsExtensionMethod)
                        {
                            referencedSymbol = referencedMethod.ReducedFrom ?? referencedSymbol;
                        }
                    }

                    if (referencedSymbol.Equals(member))
                    {
                        context.ReportDiagnostic(Diagnostic.Create(diagnostic, memberName.GetLocation()));
                    }
                }
            }
        }

        private bool TryGetMemberReferenceInfo(ExpressionSyntax expression, out ExpressionSyntax baseReference, out SimpleNameSyntax memberName)
        {
            switch (expression.Kind())
            {
                case SyntaxKind.SimpleMemberAccessExpression:
                    {
                        MemberAccessExpressionSyntax memberAccess = (MemberAccessExpressionSyntax)expression;
                        memberName = memberAccess.Name;
                        baseReference = memberAccess.Expression;
                    }

                    return true;

                case SyntaxKind.ConditionalAccessExpression:
                    {
                        ConditionalAccessExpressionSyntax conditionalAccess = (ConditionalAccessExpressionSyntax)expression;
                        ExpressionSyntax memberExpression = conditionalAccess.WhenNotNull;
                        if (memberExpression.Kind() == SyntaxKind.MemberBindingExpression)
                        {
                            memberName = ((MemberBindingExpressionSyntax)memberExpression).Name;
                            baseReference = conditionalAccess.Expression;
                            return true;
                        }
                    }

                    break;
            }

            baseReference = null;
            memberName = null;
            return false;
        }

        private System.Collections.Generic.IEnumerable<ISymbol> GetAllMembers(INamedTypeSymbol type, string name)
        {
            while (type != null)
            {
                foreach (var member in type.GetMembers(name))
                {
                    yield return member;
                }

                type = type.BaseType;
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

        private static readonly LocalizableString s_localizableTypeInfoGUIDMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeTypeInfoGUIDMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor TypeInfoGUIDDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeTypeInfoGUIDRuleId,
            s_localizableTypeInfoGUIDMessageAndTitle,
            s_localizableTypeInfoGUIDMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Info,
            isEnabledByDefault: true);

        private static readonly LocalizableString s_localizableTypeGetRuntimeMethodsMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeTypeGetRuntimeMethodsMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor TypeGetRuntimeMethodsDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeTypeGetRuntimeMethodsRuleId,
            s_localizableTypeGetRuntimeMethodsMessageAndTitle,
            s_localizableTypeGetRuntimeMethodsMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Info,
            isEnabledByDefault: true);

        private static readonly LocalizableString s_localizableTypeGetTypeMessageAndTitle = new LocalizableResourceString(nameof(RoslynDiagnosticsResources.NetNativeTypeGetTypeMessage), RoslynDiagnosticsResources.ResourceManager, typeof(RoslynDiagnosticsResources));
        public static readonly DiagnosticDescriptor TypeGetTypeDescriptor = new DiagnosticDescriptor(
            RoslynDiagnosticIds.NetNativeTypeGetTypeRuleId,
            s_localizableTypeGetTypeMessageAndTitle,
            s_localizableTypeGetTypeMessageAndTitle,
            "Reliability",
            DiagnosticSeverity.Info,
            isEnabledByDefault: true);
    }
}
