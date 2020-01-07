namespace ConwayTests

open Program
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

        CollectionAssert.AreEquivalent(expected, SampleData.world())

    [<Test>]
    let ``world can be retrieved as string array``() = 
        let expected = [
           "     "
           "  *  "
           " *** "
           "  *  "
           "     "
        ]

        let dump = Program.World.getAsText (SampleData.world())
        CollectionAssert.AreEquivalent(expected, dump)


    [<Test>]
    let ``copyAndUpdateWorld updates world with new state``() =
        let world = SampleData.world()

        let expected = [
            [ false; false; false; false; true ]
            [ false; false; true; false; false ]
            [ false; true; true; true; false ]
            [ false; false; true; false; false ]
            [ false; false; false; false; false ]
        ]

        let newWorld = World.copyAndUpdateWorld world { x = 0; y = 4; } true
        CollectionAssert.AreEquivalent(expected, newWorld)

