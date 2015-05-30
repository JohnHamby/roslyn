// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using Microsoft.CodeAnalysis.Diagnostics;
using Roslyn.Diagnostics.Analyzers.CSharp.Reliability;
using Xunit;

namespace Microsoft.CodeAnalysis.UnitTests.Performance
{
    public class NativeGatekeeperAnalyzerTests : DiagnosticAnalyzerTestBase
    {
        protected override DiagnosticAnalyzer GetCSharpDiagnosticAnalyzer()
        {
            return new NativeGatekeeperAnalyzer();
        }

        protected override DiagnosticAnalyzer GetBasicDiagnosticAnalyzer()
        {
            throw new NotImplementedException();
        }

        [Fact]
        public void MultiDimensionalArray()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;

class C
{
    int[,,,] fieldOK;
    int[,,,,] fieldTooMany;

    int[,,,] MethodOK(int[,,,] paramOK) { return null; }
    int[,,,,] MethodTooMany(int[,,,,] paramTooMany) { return null; }

    int[,,,] PropertyOK { get { return null; } }
    int[,,,,] PropertyTooMany { get { return null; } }

    void Test() 
    {
        int[,,,] localOK;
        int[,,,,] localToMany;

        object createOK = new int[,,,] { { { { } } } };
        object createTooMany = new int[,,,,] { { { { { } } } } };

        F<int[,,,]>();
        F<int[,,,,]>();
    }

    void F<T>() {}
}
";

            VerifyCSharp(
                code,
                GetCSharpResultAt(8, 8, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(11, 8, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(11, 32, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(14, 8, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(19, 12, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(22, 39, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(25, 14, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor));
        }

        [Fact]
        public void ArrayOfPointer()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;

unsafe class C
{
    int[] fieldOK;
    int*[] fieldArrayPointer;

    int[] MethodOK(int[] paramOK) { return null; }
    int*[] MethodArrayPointer(int*[] paramArrayPointer) { return null; }

    int[] PropertyOK { get { return null; } }
    int*[] PropertyArrayPointer { get { return null; } }

    void Test() 
    {
        int[] localOK;
        int*[] localArrayPointer;

        object createOK = new int[] { };
        object createArrayPointer = new int*[] { };

        F<int[]>();
        F<int*[]>();
    }

    void F<T>() {}
}
";

            VerifyCSharp(
                code,
                GetCSharpResultAt(8, 5, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(11, 5, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(11, 31, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(14, 5, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(19, 9, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(22, 41, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor),
                GetCSharpResultAt(25, 11, NativeGatekeeperAnalyzer.ArrayPointerElementDescriptor));
        }

        [Fact]
        public void IEquatableEquals()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;

class OK : System.IEquatable<OK>
{
    public bool Equals(OK other) { return true; }
    public override bool Equals(object other) { return true; }
}

class FailsToOverride : System.IEquatable<FailsToOverride>
{
    public bool Equals(FailsToOverride other) { return true; }
}
";

            VerifyCSharp(
                code,
                GetCSharpResultAt(11, 7, NativeGatekeeperAnalyzer.IEquatableEqualsDescriptor));
        }

        [Fact]
        public void ClassInterfaceAttributeValue()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Runtime.InteropServices;

[ClassInterface(ClassInterfaceType.None)]
class OK1
{
}

[ClassInterface((short)ClassInterfaceType.None)]
class OK11
{
}

[ClassInterface(ClassInterfaceType.AutoDispatch)]
class Bad1
{
}

[ClassInterface(ClassInterfaceType.AutoDual)]
class Bad2
{
}

[ClassInterface((short)ClassInterfaceType.AutoDispatch)]
class Bad11
{
}

[ClassInterface((short)ClassInterfaceType.AutoDual)]
class Bad21
{
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(16, 2, NativeGatekeeperAnalyzer.ClassInterfaceAttributeValueDescriptor),
                GetCSharpResultAt(21, 2, NativeGatekeeperAnalyzer.ClassInterfaceAttributeValueDescriptor),
                GetCSharpResultAt(26, 2, NativeGatekeeperAnalyzer.ClassInterfaceAttributeValueDescriptor),
                GetCSharpResultAt(31, 2, NativeGatekeeperAnalyzer.ClassInterfaceAttributeValueDescriptor));
        }

        [Fact]
        public void TypeInfoGUID()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.InteropServices;

class OK
{
    public int GUID { get { return 0; } }
}

class Test
{
    public void TestMethod(OK p1, TypeInfo p2, System.Type p3)
    {
        var x = p1.GUID;
        var y = p2.GUID;
        y = p2?.GUID;
        // Apparently references to GUID through System.Type are OK.
        y = p3.GUID;
        var z = p2.BaseType;
    }
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(17, 20, NativeGatekeeperAnalyzer.TypeInfoGUIDDescriptor),
                GetCSharpResultAt(18, 17, NativeGatekeeperAnalyzer.TypeInfoGUIDDescriptor));
        }
    }
}
