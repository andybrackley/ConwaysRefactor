namespace ConwayTests

open NUnit.Framework

module WorldCreation = 
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

    [<Test>]
    let ``world can be retrieved as string array``() = 
        let expected = [
           "     "
           "  *  "
           " *** "
           "  *  "
           "     "
        ]

        let dump = Program.getWorldAsText SampleData.world
        CollectionAssert.AreEquivalent(expected, dump)
