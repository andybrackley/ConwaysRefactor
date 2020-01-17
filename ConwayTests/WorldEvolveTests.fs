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
      let evolved = Program.Generations.evolve world
      let expected = [
         [ Dead; Dead; Dead; Dead; Dead; ]
         [ Dead; Alive; Alive; Alive; Dead; ]
         [ Dead; Alive; Dead; Alive; Dead; ]
         [ Dead; Alive; Alive; Alive; Dead; ]
         [ Dead; Dead; Dead; Dead; Dead; ]
      ]

      CollectionAssert.AreEquivalent(expected, evolved.Grid)


   [<TestCaseSource(typedefof<SampleInputsProvider>, "Samples")>]
   let ``evolve matches gameoflife implementation for various shapes``(sampleInput) =
      let world = World.createFromStringArray sampleInput
      let newWorld = Generations.evolve world
      World.getAsText newWorld

   [<Test>]
   let ``retrieving coordinates of neighbours gives expected results``() =
       let expected = [
          { x = 0; y = 0 };
          { x = 0; y = 1 };
          { x = 0; y = 2 };
          { x = 1; y = 0 };
          { x = 1; y = 2 };
          { x = 2; y = 0 };
          { x = 2; y = 1 };
          { x = 2; y = 2 };
       ]

       let neighbours = Generations.getNeighbourCoords { x = 1; y = 1 }
       CollectionAssert.AreEquivalent(expected, neighbours)

   [<Test>]
   let ``Sanitize Coordinates Loops From 0 to Height``() =
       let c = Generations.sanitizeCoordinate { x = -1; y = 0 } 5 5
       Assert.That(c.x, Is.EqualTo(4))
       Assert.That(c.y, Is.EqualTo(0))

   [<Test>]
   let ``Sanitize Coordinates Loops From Height to 0``() =
       let c = Generations.sanitizeCoordinate { x = 5; y = 0 } 5 5
       Assert.That(c.x, Is.EqualTo(0))
       Assert.That(c.y, Is.EqualTo(0))

   [<Test>]
   let ``Sanitize Coordinates Loops From 0 to Width``() =
       let c = Generations.sanitizeCoordinate { x = 0; y = -1 } 5 5
       Assert.That(c.x, Is.EqualTo(0))
       Assert.That(c.y, Is.EqualTo(4))

   [<Test>]
   let ``Sanitize Coordinates Loops From Width to 0``() =
       let c = Generations.sanitizeCoordinate { x = 0; y = 5 } 5 5
       Assert.That(c.x, Is.EqualTo(0))
       Assert.That(c.y, Is.EqualTo(0))

   [<Test>]
   let ``getAliveNeighbourCount returns count of only alive cells``() =
      let world = SampleData.world()
      let coord = { x = 1; y = 3 }
      let aliveNeighbourCount = Generations.getAliveNeighbourCount world 5 5 coord
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
