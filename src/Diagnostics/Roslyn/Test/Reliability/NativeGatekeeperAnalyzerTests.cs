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

        [Fact]
        public void TypeMembers()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Reflection;
using System.Reflection.RuntimeReflectionExtensions;
using System.Runtime.InteropServices;

class OK
{
    public int GetType(string s) { get { return 0; } }
    public int GetRuntimeMethods() { get { return 0; } }
}

class Test
{
    public void TestMethod(OK p1, System.Type p2, string s)
    {
        var x = p1.GetType(s);
        x = p1.GetRuntimeMethods();
        var y = p2.GetType(s);
        var z = p2.GetRuntimeMethods();
        var mumble = p2.GetEvents();
    }
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(20, 20, NativeGatekeeperAnalyzer.TypeGetTypeDescriptor),
                GetCSharpResultAt(21, 20, NativeGatekeeperAnalyzer.TypeGetRuntimeMethodsDescriptor));
        }

        [Fact]
        public void BeginEndInvoke()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;


class OK
{
    public int BeginInvoke() { get { return 0; } }
    public int EndInvoke(int x) { get { return 0; } }
}

delegate int Mumble(int);

class Test
{
    public void TestMethod(OK p1, System.Action<int> p2, System.Func<int, int> p3, Mumble p4)
    {
        var a = p1.BeginInvoke();
        p1.EndInvoke(a);
        var b = p2.BeginInvoke(3);
        p2.EndInvoke(b);
        p2.Invoke(3);
        var c = p3.BeginInvoke(3);
        p3.EndInvoke(c);
        p3.Invoke(3);
        var d = p4.BeginInvoke(3);
        p4.EndInvoke(d);
        p4.Invoke(3);
    }
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(20, 20, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor),
                GetCSharpResultAt(21, 12, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor),
                GetCSharpResultAt(23, 20, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor),
                GetCSharpResultAt(24, 12, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor),
                GetCSharpResultAt(26, 20, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor),
                GetCSharpResultAt(27, 12, NativeGatekeeperAnalyzer.BeginEndInvokeDescriptor));
        }

        [Fact]
        public void MultipleDefaultInterfaces()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;
using Windows.Foundation.Metadata;

namespace Windows.Foundation.Metadata
{
    public class DefaultAttribute : System.Attribute
    {
    }
}

[Default]
public interface I1
{
}

[Default]
public interface I2
{
}

public interface I3 : I2
{
}

public interface I4
{
}

[Default]
public interface I5: I1
{
}

class OK : I1, I4
{
}

class Multiple1 : I1, I2
{
}

class Multiple2 : I1, I3
{
}

class Sneaky: I5
{
}

class BaseLevel : I1
{
}

class MultiLevel : BaseLevel, I2
{
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(40, 7, NativeGatekeeperAnalyzer.MultipleDefaultInterfacesDescriptor),
                GetCSharpResultAt(44, 7, NativeGatekeeperAnalyzer.MultipleDefaultInterfacesDescriptor),
                GetCSharpResultAt(48, 7, NativeGatekeeperAnalyzer.MultipleDefaultInterfacesDescriptor),
                GetCSharpResultAt(56, 7, NativeGatekeeperAnalyzer.MultipleDefaultInterfacesDescriptor));
        }

        [Fact]
        public void EventSourceLocalization()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.Tracing;

[EventSource(Name = ""Fred"")]
class OK
{
}

[EventSource(LocalizationResources = ""Barney"")]
class Bad
{
}

[Custom(LocalizationResources = ""Barney"")]
[EventSource(LocalizationResources = ""Barney"")]
class Bad1
{
}

[Custom(LocalizationResources = ""Barney"")]
class OK1
{
}

public class CustomAttribute : System.Attribute
{
    public string LocalizationResources { get { return ""Wilma""; } set { } }
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(11, 2, NativeGatekeeperAnalyzer.EventSourceLocalizationDescriptor),
                GetCSharpResultAt(17, 2, NativeGatekeeperAnalyzer.EventSourceLocalizationDescriptor));
        }

        [Fact]
        public void EmptyInfiniteLoop()
        {
            var code = @"
using System.Collections.Generic;
using System.Collections.Immutable;

class Test
{
    void M(bool b, int x)
    {
        int y = 0;

        while (b)
        {
        }

        while (true)
        {
            b = !b;
        }

        while (true)
        {
        }

        while (true)
        {
            ;
        }

        while (true);

        while (true)
        {
            y = 3;
        }

        while (true)
        {
            int z = 112, zz = 22;
        }

        while (true)
        {
            int zz = y;
        }

        do { } while (true);

        do { } while (false);

        do { int aa = 10, bb, cc = x; } while (true);

        for (int i = 10; ; )
        {
        }

        for (int i = 10; true; );

        for (int i = 10; true; i++);

        for (;;)
        {
            x = 12;
        }

        for (;;)
            x = y;

        for (;;)
        {
            target: ;
        }

        for (;;)
        {
            target: x = y;
        }
    }
}
";
            VerifyCSharp(
                code,
                GetCSharpResultAt(20, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(24, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(29, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(31, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(36, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(46, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(52, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(56, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(60, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor),
                GetCSharpResultAt(68, 9, NativeGatekeeperAnalyzer.EmptyInfiniteLoopDescriptor));
        }
    }
}
