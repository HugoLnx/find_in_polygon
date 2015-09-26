package com

import geotrellis.shapefile
import geotrellis.shapefile.{Parser,Record}
import com.vividsolutions.jts.geom._

package object hugolnx {
	type Area[T] = (Geometry, T)

	class Affiliate(_id:Int) {
		def id: Int = _id
	}

	object ShapefileReader {
		def readShapefile(path:String) : Seq[Area[Affiliate]] = {
			val records:Seq[Record] = shapefile.Parser(path)

			records.map{rec => (multipolygonFrom(rec), new Affiliate(rec.id))}
		}

		def multipolygonFrom(rec:Record) : MultiPolygon = {
			val gm = new GeometryFactory()
			gm.createMultiPolygon(Array(rec.g.asInstanceOf[Polygon]))
		}
	}

	class Finder[T]() {
		def findOn(point:(Double,Double), areas:Seq[Area[T]]) : Option[T] = {
			val gm = new GeometryFactory()
			val pt = gm.createPoint(new Coordinate(point._1,point._2))
			areas.find{area =>
				try {
					pt.coveredBy(area._1)
				} catch {
					case e: TopologyException => {
						//println("" + point._1 + "," + point._2)
						true
					}
					case _ => false
				}
			}.map{area => area._2}
		}
	}

	class CellsGrid(longMin:Double, latMin:Double, longMax:Double, latMax:Double, cellSize:Int, precision:Double) {
		def longSize = coordToInt(longMax) - coordToInt(longMin)
		def latSize = coordToInt(latMax) - coordToInt(latMin)

		def qntLongCells = {
			if(longSize % cellSize == 0) {
				longSize / cellSize
			} else {
				longSize / cellSize + 1
			}
		}

		def qntLatCells = {
			if(latSize % cellSize == 0) {
				latSize / cellSize
			} else {
				latSize / cellSize + 1
			}
		}

		def longCell(long:Double) = {
			val longInt = coordToInt(long)
			val longMinInt = coordToInt(longMin)
			(longInt - longMinInt) / cellSize
		}

		def latCell(lat:Double) = {
			val latInt = coordToInt(lat)
			val latMinInt = coordToInt(latMin)
			(latInt - latMinInt) / cellSize
		}

		def eachCell(callback: (Int,Int) => _) {
			for(cellLong <- 0 until qntLongCells) {
				for(cellLat <- 0 until qntLatCells) {
					callback(cellLong, cellLat)
				}
			}
		}

		def cellPolygon(cellLong:Int, cellLat:Int) = {
			val cellLongMin = (longMin      + ( cellLong * cellSize )  / precision)
			val cellLongMax = (cellLongMin  + ( (cellSize-1)        )  / precision)
			val cellLatMin  = (latMin       + ( cellLat * cellSize  )  / precision)
			val cellLatMax  = (cellLatMin   + ( (cellSize-1)        )  / precision)

			newPolygon(
				Array(cellLongMin, cellLatMax),
				Array(cellLongMin, cellLatMin),
				Array(cellLongMax, cellLatMin),
				Array(cellLongMax, cellLatMax)
			)
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
			(math rint coord * precision).toInt
		}
	}

	object CellsGrid {
		def build[T](areas : Seq[Area[T]], cellSize:Int, precision:Double) : CellsGrid = {
			var longMin = 190.0
			var longMax = -190.0
			var latMin = 190.0
			var latMax = -190.0
			for((area,_) <- areas){
				val env = area.getEnvelopeInternal
				if(env.getMinX < longMin) longMin = env.getMinX
				if(env.getMinY < latMin ) latMin  = env.getMinY
				if(env.getMaxX > longMax) longMax = env.getMaxX
				if(env.getMaxY > latMax ) latMax  = env.getMaxY
			}

			println("longMin: " + longMin)
			println("longMax: " + longMax)
			println("latMin: " + latMin)
			println("latMax: " + latMax)
			var grid = new CellsGrid(longMin, latMin, longMax, latMax, cellSize, precision)

			println("longSize: " + grid.longSize)
			println("latSize: " + grid.latSize)
			println("longCells: " + grid.qntLongCells)
			println("latCells: " + grid.qntLatCells)
			grid
		}
	}

	class CellsMap[T](qntLongCells:Int, qntLatCells:Int) {
		var memory : Array[T] = new Array(qntLongCells*qntLatCells)

		def get(longGr: Int, latGr: Int) : Seq[Area[T]] = {
			memory(indexFor(longGr, latGr))
		}

		def set(longGr : Int, latGr : Int, value : Seq[Area[T]]) {
			memory(indexFor(longGr,latGr)) = value
		}

		def indexFor(longGr : Int, latGr : Int) : Int = {
			longGr + latGr*qntLongCells
		}
	}

	class PreciseMap(qntLongCells:Int, qntLatCells:Int) {
		var memory : Array[Boolean] = new Array(qntLongCells*qntLatCells)

		def get(longGr: Int, latGr: Int) : Boolean = {
			memory(indexFor(longGr, latGr))
		}

		def set(longGr : Int, latGr : Int, value : Boolean) {
			memory(indexFor(longGr,latGr)) = value
		}

		def indexFor(longGr : Int, latGr : Int) : Int = {
			longGr + latGr*qntLongCells
		}
	}

	class OptimalFinder[T](index:CellsIndex[T], finder:Finder[T]) {
		def find(long:Double, lat:Double) : Option[T] = {
			var areas = index.get(long, lat)
			if(index.wasPreciselyFound(long, lat)) {
				return Some(areas(0).get()._2)
			} else {
				return finder.findOn((long, lat), areas)
			}
		}
	}

	class CellsIndex[T](map:CellsMap[Seq[Area[T]]], preciseMap:PreciseMap, grid:CellsGrid) {
		def get(long:Double, lat:Double) = {
			map.get(grid.longCell(long), grid.latCell(lat))
		}

		def wasPreciselyFound(long:Double, lat:Double) : Boolean = {
			preciseMap.get(grid.longCell(long), grid.latCell(lat))
		}
	}

	object CellsIndex {
		val CELL_SIZE = 100
		val PRECISION = 3

		def to[T](areas : Seq[Area[T]]) : CellsIndex[T] = {
			val grid = CellsGrid.build(areas, CELL_SIZE, math.pow(10, PRECISION))
			val map = new CellsMap[Seq[Area[T]]](grid.qntLongCells, grid.qntLatCells)
			val preciseMap = new PreciseMap(grid.qntLongCells, grid.qntLatCells)

			grid.eachCell { (cellLong:Int, cellLat:Int) =>
				var cellPolygon = grid.cellPolygon(cellLong, cellLat)

				val selectedAreas = areas.filter{ tuple =>
					cellPolygon.intersects(tuple._1)
				}
				map.set(cellLong, cellLat, selectedAreas)
				var preciselyFound = false
				if(selectedAreas.size == 1) {
					var area = selectedAreas(0)
					try {
						if(area._1.covers(cellPolygon)) {
							preciselyFound = true
						}
					} catch {
						case e: TopologyException => println("TopologyException")
					}
				}
				preciseMap.set(cellLong, cellLat, preciselyFound)
			}
			
			return new CellsIndex(map, preciseMap, grid)
		}
	}
}
