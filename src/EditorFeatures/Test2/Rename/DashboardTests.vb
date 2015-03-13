' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Threading
Imports Microsoft.CodeAnalysis.Editor.Implementation.InlineRename
Imports Microsoft.CodeAnalysis.FindSymbols
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Shared.TestHooks

Namespace Microsoft.CodeAnalysis.Editor.UnitTests.Rename
    Public Class DashboardTests
        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithNoOverload()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameOverloads, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                using System;
                                using System.Collections.Generic;
                                using System.Linq;
                                using System.Threading.Tasks;

                                class Program
                                {
                                    public void $$foo()
                                    {
                                    }
                                }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="",
                    searchResultText:=EditorFeaturesResources.FoundReferenceInFile,
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithOverload()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameOverloads, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                using System;
                                using System.Collections.Generic;
                                using System.Linq;
                                using System.Threading.Tasks;

                                class Program
                                {
                                    public void $$foo()
                                    {
                                    }

                                    public void foo(int i)
                                    {
                                    }
                                }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 2),
                    hasRenameOverload:=True,
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <WorkItem(883263)>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithInvalidOverload()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameOverloads, True)
            VerifyDashboard(
                <Workspace>
                    <Project Language="C#" CommonReferences="true">
                        <Document>
class Program
{
    void $$X(int x)
    {
        X();
    }
    void X(int x, int y)
    {
    }
}
                            </Document>
                    </Project>
                </Workspace>,
                newName:="Bar",
                searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 2),
                changedOptionSet:=changingOptions,
                hasRenameOverload:=True,
                unresolvableConflictText:=String.Format(EditorFeaturesResources.UnresolvableConflicts, 1),
                severity:=DashboardSeverity.Error)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        <WorkItem(853839)>
        Public Sub RenameAttributeAlias()
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
using $$Evil = AttributeAttribute; 
[AttributeAttribute] 
class AttributeAttribute : System.Attribute { }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="AttributeAttributeAttribute",
                    searchResultText:=EditorFeaturesResources.FoundReferenceInFile,
                    resolvableConflictText:=String.Format(EditorFeaturesResources.ConflictsWillBeResolved, 1),
                    severity:=DashboardSeverity.Info)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        <WorkItem(700923), WorkItem(700925)>
        Public Sub RenameWithOverloadAndInStringsAndComments()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameOverloads, True)
            changingOptions.Add(RenameOptions.RenameInStrings, True)
            changingOptions.Add(RenameOptions.RenameInComments, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                using System;
                                using System.Collections.Generic;
                                using System.Linq;
                                using System.Threading.Tasks;

                                class Program
                                {
                                    public void $$foo()
                                    {
                                    }

                                    /// foo
                                    public void foo(int i)
                                    {
                                        // foo
                                        var a = "foo";
                                    }
                                }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 5),
                    hasRenameOverload:=True,
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        <WorkItem(700923), WorkItem(700925)>
        Public Sub RenameInComments()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameInComments, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                 <![CDATA[
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class $$Program
{
    /// <summary>
    /// <Program></Program>
    /// </summary>
    /// <param name="args"></param>
    static void Main(string[] args)
    {
        // Program
        /* Program!
            Program
        */
        var a = "Program";
    }
}
]]>
                             </Document>
                         </Project>
                     </Workspace>),
                    newName:="P",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 6),
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        <WorkItem(700923), WorkItem(700925)>
        Public Sub RenameInStrings()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameInStrings, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                 <![CDATA[
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class $$Program
{
    /// <summary>
    /// <Program></Program>
    /// </summary>
    /// <param name="args"></param>
    static void Main(string[] args)
    {
        // Program
        /* Program!
            Program
        */
        var a = "Program";
    }
}
]]>
                             </Document>
                         </Project>
                     </Workspace>),
                    newName:="P",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 2),
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        <WorkItem(700923), WorkItem(700925)>
        Public Sub RenameInCommentsAndStrings()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameInComments, True)
            changingOptions.Add(RenameOptions.RenameInStrings, True)
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                 <![CDATA[
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

class $$Program
{
    /// <summary>
    /// <Program></Program>
    /// </summary>
    /// <param name="args"></param>
    static void Main(string[] args)
    {
        // Program
        /* Program!
            Program
        */
        var a = "Program";
    }
}
]]>
                             </Document>
                         </Project>
                     </Workspace>),
                    newName:="P",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 7),
                    changedOptionSet:=changingOptions)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub NonConflictingEditWithMultipleLocations()
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                class $$Foo
                                {
                                    void Blah()
                                    {
                                        Foo f = new Foo();
                                    }
                                }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="",
                    searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 3))
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub NonConflictingEditWithSingleLocation()
            VerifyDashboard(
                    (<Workspace>
                         <Project Language="C#" CommonReferences="true">
                             <Document>
                                class $$UniqueClassName
                                {
                                    void Blah()
                                    {
                                        Foo f = new Foo();
                                    }
                                }
                            </Document>
                         </Project>
                     </Workspace>),
                    newName:="",
                    searchResultText:=EditorFeaturesResources.FoundReferenceInFile)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub ParameterConflictingWithInstanceField()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#">
                         <Document>
                               class Foo
                               {
                                   int foo;
                                   void Blah(int [|$$bar|])
                                   {
                                       foo = [|bar|];
                                   }
                               }
                           </Document>
                     </Project>
                 </Workspace>),
                newName:="foo",
                searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 2),
                resolvableConflictText:=String.Format(EditorFeaturesResources.ConflictsWillBeResolved, 1),
                severity:=DashboardSeverity.Info)
        End Sub

        <WorkItem(5923, "DevDiv_Projects/Roslyn")>
        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub ParameterConflictingWithInstanceFieldMoreThanOnce()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#">
                         <Document>
                               class Foo
                               {
                                   int foo;
                                   void Blah(int [|$$bar|])
                                   {
                                       foo = foo + [|bar|];
                                   }
                               }
                           </Document>
                     </Project>
                 </Workspace>),
                newName:="foo",
                searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 2),
                resolvableConflictText:=String.Format(EditorFeaturesResources.ConflictsWillBeResolved, 2),
                severity:=DashboardSeverity.Info)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub ParameterConflictingWithLocal_Unresolvable()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#">
                         <Document>
                               class Foo
                               {
                                   void Blah(int [|$$bar|])
                                   {
                                       int foo;
                                   }
                               }
                           </Document>
                     </Project>
                 </Workspace>),
                newName:="foo",
                searchResultText:=EditorFeaturesResources.FoundReferenceInFile,
                unresolvableConflictText:=String.Format(EditorFeaturesResources.UnresolvableConflicts, 1),
                severity:=DashboardSeverity.Error)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub MoreThanOneUnresolvableConflicts()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#">
                         <Document>
                               class Foo
                               {
                                   void Blah(int [|$$bar|])
                                   {
                                       int foo;
                                       foo = [|bar|];
                                       foo = [|bar|];
                                   }
                               }
                           </Document>
                     </Project>
                 </Workspace>),
                newName:="foo",
                searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 3),
                unresolvableConflictText:=String.Format(EditorFeaturesResources.UnresolvableConflicts, 3),
                severity:=DashboardSeverity.Error)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub ConflictsAcrossLanguages_Resolvable()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#" AssemblyName="CSharpAssembly" CommonReferences="true">
                         <Document>
                               namespace N
                               {
                                    public class [|$$Foo|]
                                    {
                                        void Blah()
                                        {
                                            [|Foo|] f = new [|Foo|]();
                                        }
                                    }
                               }
                           </Document>
                     </Project>
                     <Project Language="Visual Basic" CommonReferences="true">
                         <ProjectReference>CSharpAssembly</ProjectReference>
                         <Document>
                               Imports N
                               Class Bar
                                   Sub Blah()
                                      Dim f = new {|N.Foo:Foo|}()
                                   End Sub
                               End Class
                           </Document>
                     </Project>
                 </Workspace>),
                   newName:="Bar",
                   searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInMultipleFiles, 4, 2),
                   resolvableConflictText:=String.Format(EditorFeaturesResources.ConflictsWillBeResolved, 1),
                   severity:=DashboardSeverity.Info)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithNameof_FromDefinition_DoesNotForceRenameOverloadsOption()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#" AssemblyName="CSharpAssembly" CommonReferences="true">
                         <Document>
class C
{
    void M$$()
    {
        nameof(M).ToString();
    }
    void M(int x) { }
}
                        </Document>
                     </Project>
                 </Workspace>),
                   newName:="Mo",
                   searchResultText:=String.Format(EditorFeaturesResources.FoundReferenceInFile),
                   hasRenameOverload:=True,
                   isRenameOverloadsEditable:=True)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithNameof_FromReference_DoesForceRenameOverloadsOption()
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#" AssemblyName="CSharpAssembly" CommonReferences="true">
                         <Document>
class C
{
    void M()
    {
        nameof(M$$).ToString();
    }
    void M(int x) { }
}
                        </Document>
                     </Project>
                 </Workspace>),
                   newName:="Mo",
                   searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 3),
                   hasRenameOverload:=True,
                   isRenameOverloadsEditable:=False)
        End Sub

        <Fact>
        <Trait(Traits.Feature, Traits.Features.Rename)>
        Public Sub RenameWithNameof_FromDefinition_WithRenameOverloads_Cascading()
            Dim changingOptions = New Dictionary(Of OptionKey, Object)()
            changingOptions.Add(RenameOptions.RenameOverloads, True)
            VerifyDashboard(
                (<Workspace>
                     <Project Language="C#" AssemblyName="CSharpAssembly" CommonReferences="true">
                         <Document>
class B
{
    public virtual void [|M|](int x)
    {
        nameof([|M|]).ToString();
    }
}

class D : B
{
    public void $$[|M|]()
    {
        nameof([|M|]).ToString();
    }

    public override void [|M|](int x)
    {
    }
}
                        </Document>
                     </Project>
                 </Workspace>),
                   newName:="Mo",
                   searchResultText:=String.Format(EditorFeaturesResources.FoundReferencesInFile, 5),
                   changedOptionSet:=changingOptions,
                   hasRenameOverload:=True)
        End Sub

        Friend Shared Sub VerifyDashboard(
            test As XElement,
            newName As String,
            searchResultText As String,
            Optional hasRenameOverload As Boolean = False,
            Optional isRenameOverloadsEditable As Boolean = True,
            Optional changedOptionSet As Dictionary(Of OptionKey, Object) = Nothing,
            Optional resolvableConflictText As String = Nothing,
            Optional unresolvableConflictText As String = Nothing,
            Optional severity As DashboardSeverity = DashboardSeverity.None
        )

            Using workspace = CreateWorkspaceWithWaiter(test)
                Dim cursorDocument = workspace.Documents.Single(Function(d) d.CursorPosition.HasValue)
                Dim cursorPosition = cursorDocument.CursorPosition.Value

                Dim document = workspace.CurrentSolution.GetDocument(cursorDocument.Id)
                Assert.NotNull(document)

                Dim token = document.GetSyntaxTreeAsync().Result.GetRoot().FindToken(cursorPosition)

                Dim renameService = DirectCast(workspace.GetService(Of IInlineRenameService)(), InlineRenameService)

                ' Create views for all documents to ensure that undo is hooked up properly
                For Each d In workspace.Documents
                    d.GetTextView()
                Next

                Dim optionSet = workspace.Options

                If changedOptionSet IsNot Nothing Then
                    For Each entry In changedOptionSet
                        optionSet = optionSet.WithChangedOption(entry.Key, entry.Value)
                    Next
                End If

                workspace.Services.GetService(Of IOptionService)().SetOptions(optionSet)

                Dim sessionInfo = renameService.StartInlineSession(
                    document, document.GetSyntaxTreeAsync().Result.GetRoot().FindToken(cursorPosition).Span, CancellationToken.None)

                ' Perform the edit in the buffer
                Using edit = cursorDocument.TextBuffer.CreateEdit()
                    edit.Replace(token.SpanStart, token.Span.Length, newName)
                    edit.Apply()
                End Using

                Dim listeners = DirectCast(workspace.ExportProvider.GetExports(Of IAsynchronousOperationListener, FeatureMetadata)(), IEnumerable(Of Lazy(Of IAsynchronousOperationListener, FeatureMetadata)))
                Dim renameListener = New AggregateAsynchronousOperationListener(listeners, FeatureAttribute.Rename)

                Using dashboard = New Dashboard(New DashboardViewModel(DirectCast(sessionInfo.Session, InlineRenameSession)), cursorDocument.GetTextView())
                    WaitForRename(workspace)

                    Dim model = DirectCast(dashboard.DataContext, DashboardViewModel)

                    Assert.Equal(searchResultText, model.SearchText)

                    If String.IsNullOrEmpty(resolvableConflictText) Then
                        Assert.False(model.HasResolvableConflicts, "Expected no resolvable conflicts")
                        Assert.Null(model.ResolvableConflictText)
                    Else
                        Assert.True(model.HasResolvableConflicts, "Expected resolvable conflicts")
                        Assert.Equal(resolvableConflictText, model.ResolvableConflictText)
                    End If

                    If String.IsNullOrEmpty(unresolvableConflictText) Then
                        Assert.False(model.HasUnresolvableConflicts, "Expected no unresolvable conflicts")
                        Assert.Null(model.UnresolvableConflictText)
                    Else
                        Assert.True(model.HasUnresolvableConflicts, "Expected unresolvable conflicts")
                        Assert.Equal(unresolvableConflictText, model.UnresolvableConflictText)
                    End If

                    Assert.Equal(hasRenameOverload, model.Session.HasRenameOverloads)
                    Assert.Equal(isRenameOverloadsEditable, model.IsRenameOverloadsEditable)
                    If Not isRenameOverloadsEditable Then
                        Assert.True(model.DefaultRenameOverloadFlag)
                    End If

                    Assert.Equal(severity, model.Severity)
                End Using

                sessionInfo.Session.Cancel()
            End Using
        End Sub

    End Class
End Namespace
