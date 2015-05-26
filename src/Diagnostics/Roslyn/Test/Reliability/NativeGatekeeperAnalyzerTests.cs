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
    }
}
