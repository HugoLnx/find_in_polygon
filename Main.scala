import com.hugolnx._
import java.io._

object Main {
	// longMin: -73.9904499689999
	// longMax: -22.256396161353557
	// latMin: -41.29240741890326
	// latMax: 13.864926899496986
	def main(args: Array[String]) {
		// val affiliates = ShapefileReader.readShapefile("./channel_map_2015_v6/channel_map_2015_v6.shp")
		// val finder = new Finder[Affiliate]()
		// var i = 0
		// for(longInt <- -73991 until -22258) {
		// 	for(latInt <- -41293 until 13865) {
		// 		var pt = (longInt/1000.0, latInt/1000.0)
		// 		var optAffiliate = finder.findOn(pt, affiliates)
		// 		var id = optAffiliate.map{affiliate=>affiliate.id}.getOrElse(null)
		// 		val oos = new ObjectOutputStream(new FileOutputStream("./res/test/fixtures/affiliates/pos" + longInt + "x" + latInt + ".txt"))
		// 		oos.writeObject(id)
		// 		oos.close
		// 		if(i % 100000 == 0) {
		// 			println(i)
		// 		}
		// 		i += 1
		// 	}
		// }
	}

	def getAffiliate() : Option[Affiliate] = {
		val index = getIndex
		val finder = new Finder[Affiliate]()
		val optFinder = new OptimalFinder[Affiliate](index, finder)
		val pt = (-46.60, -23.30)
		optFinder.find(pt._1, pt._2)
	}

	def getIndex() : CellsIndex[Affiliate] = {
		val affiliates = ShapefileReader.readShapefile("./channel_map_2015_v6/channel_map_2015_v6.shp")

		CellsIndex.to(affiliates)
	}
}
