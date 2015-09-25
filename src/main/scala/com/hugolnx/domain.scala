package com.hugolnx

import geotrellis.shapefile
import geotrellis.shapefile.{Parser,Record}
import com.vividsolutions.jts.geom._

class Affiliate(_id:Int, _area:Geometry) {
	def id: Int = _id
	def area: Geometry = _area
}

object ShapefileReader {
	def readShapefile(path:String) : Seq[Affiliate] = {
		val records:Seq[Record] = shapefile.Parser(path)

		records.map{rec => new Affiliate(rec.id, multipolygonFrom(rec))}
	}

	def multipolygonFrom(rec:Record) : MultiPolygon = {
		val gm = new GeometryFactory()
		gm.createMultiPolygon(Array(rec.g.asInstanceOf[Polygon]))
	}
}

class CellsIndex(longSize : Int, latSize : Int, longMin : Double, latMin : Double) {
	var index : Map[Array[Int],Seq[Affiliate]] = Map()//new Array[Seq[Affiliate]](longSize*latSize)

	def get(long: Double, lat: Double) : Seq[Affiliate] = {
		index(Array(longGr(long), latGr(lat)))
		//index(inxFor(longGr(long), latGr(lat)))
	}

	def longGr(long:Double) = (CellsIndex.coordToInt(long) - CellsIndex.coordToInt(longMin)) / CellsIndex.CELL_SIZE
	def latGr(lat:Double) = (CellsIndex.coordToInt(lat) - CellsIndex.coordToInt(latMin)) / CellsIndex.CELL_SIZE

	def set(longGr : Int, latGr : Int, affiliates : Seq[Affiliate]) {
		//index(inxFor(longGr,latGr)) = affiliates
		index += (Array(longGr,latGr) -> affiliates)
	}

	def inxFor(longGr : Int, latGr : Int) : Int = longGr + latGr*longSize
}

object CellsIndex {
	val CELL_SIZE = 1000

	def to(all_affiliates : Seq[Affiliate]) : CellsIndex = {
		var affiliates = all_affiliates
		var longMin = 190.0
		var longMax = -190.0
		var latMin = 190.0
		var latMax = -190.0
		for(affiliate <- affiliates){
			val env = affiliate.area.getEnvelopeInternal
			if(env.getMinX < longMin) longMin = env.getMinX
			if(env.getMinY < latMin ) latMin  = env.getMinY
			if(env.getMaxX > longMax) longMax = env.getMaxX
			if(env.getMaxY > latMax ) latMax  = env.getMaxY
		}
		
		val longSize = coordToInt(longMax) - coordToInt(longMin)
		val latSize = coordToInt(latMax) - coordToInt(latMin)

		val qntLongCells = if(longSize % CELL_SIZE == 0) longSize / CELL_SIZE else longSize / CELL_SIZE + 1
		val qntLatCells = if(latSize % CELL_SIZE == 0) latSize / CELL_SIZE else latSize / CELL_SIZE + 1
		println("longMin: " + longMin)
		println("longMax: " + longMax)
		println("latMin: " + latMin)
		println("latMax: " + latMax)
		println("longSize: " + longSize)
		println("latSize: " + latSize)
		println("longCells: " + qntLongCells)
		println("latCells: " + qntLatCells)
		val index = new CellsIndex(qntLongCells, qntLatCells, longMin, latMin)

		for(longGr <- 0 until qntLongCells) {
			for(latGr <- 0 until qntLatCells) {
				val localLongMin = (longMin      + ( longGr * CELL_SIZE )  / 1000.0)
				val localLongMax = (localLongMin + ( (CELL_SIZE-1)      )  / 1000.0)
				val localLatMin =  (latMin       + ( latGr * CELL_SIZE  )  / 1000.0)
				val localLatMax =  (localLatMin  + ( (CELL_SIZE-1)      )  / 1000.0)

				var cell = newPolygon(
					Array(localLongMin, localLatMax),
					Array(localLongMin, localLatMin),
					Array(localLongMax, localLatMin),
					Array(localLongMax, localLatMax)
				)

				val selectedAffiliates = affiliates.filter{affiliate =>
					try {
						cell.crosses(affiliate.area)
					} catch {
						case ex: TopologyException => {
							//println("Crosses exception in Affiliate " + affiliate.id + " and cell " + cell)
							true
						}
					}
		    }
				index.set(longGr, latGr, selectedAffiliates)
			}
		}
		
		return index
	}

	def newPolygon(pt1 : Array[Double], pt2 : Array[Double], pt3 : Array[Double], pt4 : Array[Double]) : Polygon = {
		val gm = new GeometryFactory()
		gm.createPolygon(gm.createLinearRing(Array(
			new Coordinate(pt1(0), pt1(1)),
			new Coordinate(pt2(0), pt2(1)),
			new Coordinate(pt3(0), pt3(1)),
			new Coordinate(pt4(0), pt4(1)),
			new Coordinate(pt1(0), pt1(1))
		)))
	}

	def coordToInt(coord: Double) : Int = {
		(math rint coord * 1000).toInt
	}
}
