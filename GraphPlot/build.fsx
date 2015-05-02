#!packages/FAKE/tools/FAKE.exe
#r @"packages/FAKE/tools/FakeLib.dll"
open Fake

let buildDir = "./build/"

Target "Clean" (fun _ ->
  CleanDirs [buildDir]
)

Target "BuildApp" (fun _ ->
  !! "src/**/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: "
)

Target "Default" (fun _ ->
  trace "Default Target."
)

"Clean"
  ==> "BuildApp"
  ==> "Default"

RunTargetOrDefault "Default"
