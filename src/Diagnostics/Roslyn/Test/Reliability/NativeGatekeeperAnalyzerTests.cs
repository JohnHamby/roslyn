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
    int[,,,,] MethodTooMany(int[,,,,] paramOK) { return null; }

    int[,,,] PropertyOK { get { return null; } }
    int[,,,,] PropertyTooLong { get { return null; } }

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
                GetCSharpResultAt(8, 5, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(11, 5, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(11, 29, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(14, 5, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(19, 9, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(22, 36, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor),
                GetCSharpResultAt(25, 11, NativeGatekeeperAnalyzer.ArrayMoreThanFourDimensionsDescriptor));
        }
    }
}
