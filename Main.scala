import com.hugolnx._

object Main {
	def main(args: Array[String]) {
		val index = CellsIndex
	}

	def getIndex() : CellsIndex = {
		val affiliates = ShapefileReader.readShapefile("./channel_map_2015_v6/channel_map_2015_v6.shp")

		CellsIndex.to(affiliates)
	}
}
