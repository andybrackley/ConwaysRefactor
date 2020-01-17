namespace ConwayTests

open Program
open NUnit.Framework

module WorldCreation = 
    open Cell

    [<Test>]
    let ``Given the sample input the world is created as expected`` () =
        let expected = [
            [ Dead; Dead; Dead; Dead; Dead ]
            [ Dead; Dead; Alive; Dead; Dead ]
            [ Dead; Alive; Alive; Alive; Dead ]
            [ Dead; Dead; Alive; Dead; Dead ]
            [ Dead; Dead; Dead; Dead; Dead ]
        ]

        CollectionAssert.AreEquivalent(expected, SampleData.world().Grid)

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
            [ Dead; Dead; Dead; Dead; Alive ]
            [ Dead; Dead; Alive; Dead; Dead ]
            [ Dead; Alive; Alive; Alive; Dead ]
            [ Dead; Dead; Alive; Dead; Dead ]
            [ Dead; Dead; Dead; Dead; Dead ]
        ]

        let newWorld = World.copyAndUpdateWorld world { x = 0; y = 4; } Alive
        CollectionAssert.AreEquivalent(expected, newWorld.Grid)

