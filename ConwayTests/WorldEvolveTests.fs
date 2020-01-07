namespace ConwayTests

open Program
open NUnit.Framework

module WorldEvolveTests = 
   [<Test>]
   let ``On evolve new world is created``() = 
      let world = SampleData.world()
      let evolved = Program.Generations.evolve world
      let expected = [
         [ false; false; false; true; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
      ]

      CollectionAssert.AreEquivalent(expected, world)

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



