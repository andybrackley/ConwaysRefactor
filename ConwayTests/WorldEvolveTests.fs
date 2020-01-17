namespace ConwayTests

open Program
open NUnit.Framework

module WorldEvolveTests = 
   open Cell

   // Generated the Samples from this online gameoflife utility
   type SampleInputsProvider() = 
      static member Samples with get() =
         [  
            TestCaseData([|
                "_____";
                "__*__";
                "_***_";
                "__*__";
                "_____";
            |]).Returns([|
               "     ";
               " *** ";
               " * * ";
               " *** ";
               "     ";
            |]);

            TestCaseData([|
                "     ";
                " *** ";
                " * * ";
                " *** ";
                "     ";
            |]).Returns([|
               "  *  ";
               " * * ";
               "*   *";
               " * * ";
               "  *  ";
            |]);

            TestCaseData([|
               "  *  ";
               " * * ";
               "*   *";
               " * * ";
               "  *  ";
            |]).Returns([|
               " *** ";
               "*****";
               "** **";
               "*****";
               " *** ";
            |]);

            TestCaseData([|
                " *** ";
                "*****";
                "** **";
                "*****";
                " *** ";
            |]).Returns([|
               "     ";
               "     ";
               "     ";
               "     ";
               "     ";
            |]);

            TestCaseData([|
                "_____";
                "__*__";
                "__*__";
                "__*__";
                "_____";
            |]).Returns([|
               "     ";
               "     ";
               " *** ";
               "     ";
               "     ";
            |]);

            TestCaseData([|
                "     ";
                "     ";
                " *** ";
                "     ";
                "     ";
            |]).Returns([|
               "     ";
               "  *  ";
               "  *  ";
               "  *  ";
               "     ";
            |]);
         ]

   [<Test>]
   let ``On evolve new world is created``() = 
      let world = SampleData.world()
      let evolved = Program.Generations.evolveWorld world
      let expected = [
         [ Dead; Dead; Dead; Dead; Dead; ]
         [ Dead; Alive; Alive; Alive; Dead; ]
         [ Dead; Alive; Dead; Alive; Dead; ]
         [ Dead; Alive; Alive; Alive; Dead; ]
         [ Dead; Dead; Dead; Dead; Dead; ]
      ]

      CollectionAssert.AreEquivalent(expected, evolved |> World.getAs2dArray)


   [<TestCaseSource(typedefof<SampleInputsProvider>, "Samples")>]
   let ``evolve matches gameoflife implementation for various shapes``(sampleInput) =
      let world = World.createFromStringArray sampleInput
      world 
        |> World.getAsText
        |> Seq.iter(fun l -> System.Diagnostics.Debug.WriteLine(sprintf "%s" l))

      let newWorld = Generations.evolveWorld world
      let asText = World.getAsText newWorld
      asText |> Seq.iter(fun l -> System.Diagnostics.Debug.WriteLine(sprintf "%s" l))
      asText

   [<Test>]
   let ``retrieving coordinates of neighbours gives expected results``() =
       let expected = [
          (0, 0);
          (0, 1);
          (0, 2);
          (1, 0);
          (1, 2);
          (2, 0);
          (2, 1);
          (2, 2);
       ]

       let expected = expected |> List.map(fun (r, c) -> { x = Row(r); y = Col(c) })

       let world = SampleData.world()
       let neighbours = Generations.getNeighbourCoords world { x = Row(1); y = Col(1) }
       CollectionAssert.AreEquivalent(expected, neighbours)

   [<Test>]
   let ``getAliveNeighbourCount returns count of only alive cells``() =
      let world = SampleData.world()
      let coord = { x = Row(1); y = Col(3) }
      let aliveNeighbourCount = Generations.getAliveNeighbourCount world coord
      Assert.That(aliveNeighbourCount, Is.EqualTo(3))

   [<TestCase(1, ExpectedResult = false)>]
   [<TestCase(2, ExpectedResult = true)>]
   [<TestCase(3, ExpectedResult = true)>]
   [<TestCase(4, ExpectedResult = false)>]
   let ``given a live current state and neighbour count newState is correct``(neighbours) =
      Generations.newState Alive neighbours
      |> isAlive

   [<TestCase(1, ExpectedResult = false)>]
   [<TestCase(2, ExpectedResult = false)>]
   [<TestCase(3, ExpectedResult = true)>]
   [<TestCase(4, ExpectedResult = false)>]
   let ``given a dead current state and neighbour count newState is correct``(neighbours) =
      Generations.newState Dead neighbours
      |> isAlive
