namespace ConwayTests

open NUnit.Framework

module WorldEvolveTests = 
   [<Test>]
   let ``On evolve new world is created``() = 
      let world = SampleData.world
      let evolved = Program.evolve world
      let expected = [
         [ false; false; false; true; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
         [ false; false; false; false; false; ]
      ]

      CollectionAssert.AreEquivalent(expected, world)

