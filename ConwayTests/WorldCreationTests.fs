module ConwayTests

open NUnit.Framework

module SampleData = 
    let input = """
_____
__*__
_***_
__*__
_____
"""

    let world = Program.createWorldFromString input 


[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Given the sample input the world is created as expected`` () =
    let expected = [
        [ false; false; false; false; false ]
        [ false; false; true; false; false ]
        [ false; true; true; true; false ]
        [ false; false; true; false; false ]
        [ false; false; false; false; false ]
    ]

    CollectionAssert.AreEquivalent(expected, SampleData.world)