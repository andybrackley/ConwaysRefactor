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

        CollectionAssert.AreEquivalent(expected, SampleData.world() |> World.getAs2dArray)

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

