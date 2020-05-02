package com.guncolony.pvz2levelmaker

import java.text.DecimalFormat
import java.util

import com.google.gson.{JsonArray, JsonObject, JsonPrimitive}
import com.google.gson.reflect.TypeToken
import com.guncolony.pvz2levelmaker.Wave.{DoubleTextField, IntTextField, Module, StringTextField, ZombieData, getAlias, gson, requestJsonUpdate, toFriendlyZombieName}
import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.text.Font
import scalafx.Includes._

import scala.jdk.CollectionConverters._

/**
 * Contains modules to edit the ambushes in the game
 *
 * Remember to update Wave.updateModules() when adding a new module
 */
object AmbushModules {
  val formatter = new DecimalFormat("##.###")

  class StormModule (override val json: JsonObject, var columnStart: Int, var columnEnd: Int, var groupSize: Int,
                     var timeBetweenGroups: Double, var Zombies: java.util.ArrayList[ZombieData]) extends Module (json) {
    override def toString: String = "StormModule - " + Zombies.toString
    override def getZombieTypes: Set[String] = Zombies.asScala.map(_.Type_()).map(toFriendlyZombieName).toSet

    override def getCurrentJson: JsonObject = {
      // Note - this has the side effect of changing the relevant fields in variable--json
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("ColumnStart", new JsonPrimitive(columnStart))
      objdata.add("ColumnEnd", new JsonPrimitive(columnEnd))
      objdata.add("GroupSize", new JsonPrimitive(groupSize))
      objdata.add("TimeBetweenGroups",
        if (timeBetweenGroups == timeBetweenGroups.floor) new JsonPrimitive(timeBetweenGroups.toInt)
        else new JsonPrimitive(timeBetweenGroups)) // Have decimals only if needed
      objdata.add("Zombies", gson.toJsonTree(Zombies,
        new TypeToken[util.ArrayList[ZombieData]]{}.getType))
      json
    }

    override def getDisplayNode: Node = {
      new VBox {
        val editor: WaveEditor = new WaveEditor {
          text = ZombieData.getZombieText(Zombies.asScala)
          font = new Font("Courier New", 13)
          wrapText = true
          prefRowCount = 4
          text

          text.addListener((_,_,newText) => {
            Zombies = ZombieData.updateFromZombieText(newText)
            requestJsonUpdate()
          })
        }

        children = Seq(new HBox {
          children = Seq(new Label(getAlias(json) + " - Storm Ambush | Columns (0-8): "),
            new IntTextField(columnStart, v => {columnStart = v; requestJsonUpdate()}),
            new Label(" - "),
            new IntTextField(columnEnd, v => {columnEnd = v; requestJsonUpdate()}),
            new Label(" Group Size: "),
            new IntTextField(groupSize, v => {groupSize = v; requestJsonUpdate()}),
            new Label(" Time Between Groups: "),
            new DoubleTextField(timeBetweenGroups, v => {timeBetweenGroups = v; requestJsonUpdate()}),
          )
        }, editor)
      }
    }
  }
  def emptyStormModule(stormType: String): StormModule = {
    val json = new JsonObject()
    json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
    json.add("objclass", new JsonPrimitive("StormZombieSpawnerProps"))
    val objdata = new JsonObject()
    json.add("objdata", objdata)
    objdata.add("ColumnEnd", new JsonPrimitive(7))
    objdata.add("ColumnStart", new JsonPrimitive(5))
    objdata.add("GroupSize", new JsonPrimitive(1))
    objdata.add("TimeBetweenGroups", new JsonPrimitive(2))
    objdata.add("Type", new JsonPrimitive(stormType))
    objdata.add("Zombies", new JsonArray())
    new StormModule(json, 5, 7, 1, 2, new util.ArrayList[ZombieData])
  }

  // Parachute Rain
  class ParachuteRainModule (override val json: JsonObject, var columnStart: Int, var columnEnd: Int,
                   var groupSize: Int, var spiderCount: Int, var spiderZombieName: String,
                   var timeBetweenGroups: Double) extends Module(json) {
    override def toString: String = "ParachuteRainModule - " + spiderZombieName
    override def getZombieTypes: Set[String] = Array(spiderZombieName).toSet

    override def getCurrentJson: JsonObject = {
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("ColumnStart", new JsonPrimitive(columnStart))
      objdata.add("ColumnEnd", new JsonPrimitive(columnEnd))
      objdata.add("SpiderCount", new JsonPrimitive(spiderCount))
      objdata.add("GroupSize", new JsonPrimitive(groupSize))
      objdata.add("TimeBetweenGroups", new JsonPrimitive(formatter.format(timeBetweenGroups)))
      // Above is stored as String in vanilla JSONs
      objdata.add("SpiderZombieName", new JsonPrimitive(spiderZombieName))
      json
    }

    override def getDisplayNode: Node = {
      new VBox {

        children = Seq(new HBox {
          children = Seq(new Label(getAlias(json) + " - Parachute Rain | Columns (0-8): "),
            new IntTextField(columnStart, v => {columnStart = v; requestJsonUpdate()}),
            new Label(" - "),
            new IntTextField(columnEnd, v => {columnEnd = v; requestJsonUpdate()}),
            new Label(" Total Count: "),
            new IntTextField(spiderCount, v => {spiderCount = v; requestJsonUpdate()}),
            new Label(" Group Size: "),
            new IntTextField(groupSize, v => {groupSize = v; requestJsonUpdate()}),
          )
        }, new HBox {
          children = Seq(new Label("        Zombie Type: "),
            new StringTextField(spiderZombieName, v => {spiderZombieName = v; requestJsonUpdate()}) {
              prefWidth = 150
            },
            new Label(" Time Between Groups: "),
            new DoubleTextField(timeBetweenGroups, v => {timeBetweenGroups = v; requestJsonUpdate()})
          )
        })
      }
    }
  }
  def emptyParachuteRainModule(): ParachuteRainModule = {
    val json = new JsonObject()
    json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
    json.add("objclass", new JsonPrimitive("ParachuteRainZombieSpawnerProps"))
    val objdata = new JsonObject()
    json.add("objdata", objdata)
    objdata.add("ColumnEnd", new JsonPrimitive("7"))
    objdata.add("ColumnStart", new JsonPrimitive("5"))
    objdata.add("GroupSize", new JsonPrimitive("2"))
    objdata.add("SpiderCount", new JsonPrimitive("2"))
    objdata.add("SpiderZombieName", new JsonPrimitive("lostcity_lostpilot"))
    objdata.add("TimeBeforeFullSpawn", new JsonPrimitive("1"))
    objdata.add("TimeBetweenGroups", new JsonPrimitive("0.2"))
    objdata.add("WaveStartMessage", new JsonPrimitive("[WARNING_PARACHUTERAIN]"))
    objdata.add("ZombieFallTime", new JsonPrimitive("1.5"))
    new ParachuteRainModule(json, 7, 5, 2, 2, "lostcity_lostpilot", 0.2)
  }

  // Raiding Party
  class RaidingPartyModule (override val json: JsonObject, var groupSize: Int, var swashbucklerCount: Int,
                             var timeBetweenGroups: Double) extends Module(json) {
    override def toString: String = "RaidingPartyModule - Count " + swashbucklerCount
    override def getZombieTypes: Set[String] = Array("swashbuckler").toSet

    override def getCurrentJson: JsonObject = {
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("SwashbucklerCount", new JsonPrimitive(swashbucklerCount))
      objdata.add("GroupSize", new JsonPrimitive(groupSize))
      objdata.add("TimeBetweenGroups",
        if (timeBetweenGroups == timeBetweenGroups.floor) new JsonPrimitive(timeBetweenGroups.toInt)
        else new JsonPrimitive(timeBetweenGroups)) // Have decimals only if needed
      json
    }

    override def getDisplayNode: Node = {
      new VBox {

        children = Seq(new HBox {
          children = Seq(new Label(getAlias(json) + " - Raiding Party | Swashbuckler Count: "),
            new IntTextField(swashbucklerCount, v => {swashbucklerCount = v; requestJsonUpdate()}),
            new Label(" Group Size: "),
            new IntTextField(groupSize, v => {groupSize = v; requestJsonUpdate()}),
            new Label(" Time Between Groups: "),
            new DoubleTextField(timeBetweenGroups, v => {timeBetweenGroups = v; requestJsonUpdate()})
          )
        })
      }
    }
  }
  def emptyRaidingPartyModule(): RaidingPartyModule = {
    val json = new JsonObject()
    json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
    json.add("objclass", new JsonPrimitive("RaidingPartyZombieSpawnerProps"))
    val objdata = new JsonObject()
    json.add("objdata", objdata)
    objdata.add("GroupSize", new JsonPrimitive(2))
    objdata.add("SwashbucklerCount", new JsonPrimitive(4))
    objdata.add("TimeBetweenGroups", new JsonPrimitive(1))
    new RaidingPartyModule(json, 2, 4, 1)
  }

  // Low Tide
  class LowTideModule (override val json: JsonObject, var columnStart: Int, var columnEnd: Int,
                             var groupSize: Int, var zombieCount: Int, var zombieName: String,
                             var timeBetweenGroups: Double, var timeBeforeFullSpawn: Double) extends Module(json) {
    override def toString: String = "LowTideModule - " + zombieName
    override def getZombieTypes: Set[String] = Array(zombieName).toSet

    override def getCurrentJson: JsonObject = {
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("ColumnStart", new JsonPrimitive(columnStart))
      objdata.add("ColumnEnd", new JsonPrimitive(columnEnd))
      objdata.add("ZombieCount", new JsonPrimitive(zombieCount))
      objdata.add("GroupSize", new JsonPrimitive(groupSize))
      objdata.add("TimeBetweenGroups", new JsonPrimitive(formatter.format(timeBetweenGroups)))
      objdata.add("TimeBeforeFullSpawn", new JsonPrimitive(formatter.format(timeBeforeFullSpawn)))
      objdata.add("ZombieName", new JsonPrimitive(zombieName))
      json
    }

    override def getDisplayNode: Node = {
      new VBox {

        children = Seq(new HBox {
          children = Seq(new Label(getAlias(json) + " - Low Tide | Columns (0-8): "),
            new IntTextField(columnStart, v => {columnStart = v; requestJsonUpdate()}),
            new Label(" - "),
            new IntTextField(columnEnd, v => {columnEnd = v; requestJsonUpdate()}),
            new Label(" Total Count: "),
            new IntTextField(zombieCount, v => {zombieCount = v; requestJsonUpdate()}),
            new Label(" Group Size: "),
            new IntTextField(groupSize, v => {groupSize = v; requestJsonUpdate()}),
          )
        }, new HBox {
          children = Seq(new Label("        Zombie Type: "),
            new StringTextField(zombieName, v => {zombieName = v; requestJsonUpdate()}) {
              prefWidth = 150
            },
            new Label(" Time Between Groups: "),
            new DoubleTextField(timeBetweenGroups, v => {timeBetweenGroups = v; requestJsonUpdate()}),
            new Label(" Time Before Full Spawn: "),
            new DoubleTextField(timeBeforeFullSpawn, v => {timeBeforeFullSpawn = v; requestJsonUpdate()})
          )
        })
      }
    }
  }
  def emptyLowTideModule(): LowTideModule = {
    val json = new JsonObject()
    json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
    json.add("objclass", new JsonPrimitive("BeachStageEventZombieSpawnerProps"))
    val objdata = new JsonObject()
    json.add("objdata", objdata)
    objdata.add("ColumnEnd", new JsonPrimitive(7))
    objdata.add("ColumnStart", new JsonPrimitive(5))
    objdata.add("GroupSize", new JsonPrimitive(2))
    objdata.add("TimeBeforeFullSpawn", new JsonPrimitive(1))
    objdata.add("TimeBetweenGroups", new JsonPrimitive(0.5))
    objdata.add("WaveStartMessage", new JsonPrimitive("[WARNING_LOW_TIDE]"))
    objdata.add("ZombieCount", new JsonPrimitive(2))
    objdata.add("ZombieName", new JsonPrimitive("beach"))
    new LowTideModule(json, 7, 5, 2, 2, "beach", 0.5, 1)
  }

  // Tidal Change
  class TidalChangeModule (override val json: JsonObject, var changeAmount: Int,
                           var changeType: String) extends Module(json) {
    override def toString: String = "TidalChangeModule - Change " + changeAmount

    override def getCurrentJson: JsonObject = {
      val tidal: JsonObject = json.get("objdata").getAsJsonObject.get("TidalChange").getAsJsonObject
      tidal.add("ChangeAmount", new JsonPrimitive(changeAmount))
      tidal.add("ChangeType", new JsonPrimitive(changeType))
      json
    }

    override def getDisplayNode: Node = {
      new VBox {

        children = Seq(new HBox {
          children = Seq(new Label(getAlias(json) + " - Tidal Change | Change Amount: "),
            new IntTextField(changeAmount, v => {changeAmount = v; requestJsonUpdate()}),
            new Label(" Change Type: "),
            new StringTextField(changeType, v => {changeType = v; requestJsonUpdate()}) {
              prefWidth = 90
            }
          )
        })
      }
    }
  }
  def emptyTidalChangeModule(): TidalChangeModule = {
    val json = new JsonObject()
    json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
    json.add("objclass", new JsonPrimitive("TidalChangeWaveActionProps"))
    val objdata = new JsonObject()
    json.add("objdata", objdata)
    val tidal = new JsonObject()
    objdata.add("TidalChange", tidal)
    tidal.add("ChangeAmount", new JsonPrimitive(5))
    tidal.add("ChangeType", new JsonPrimitive("absolute"))
    new TidalChangeModule(json, 5, "absolute")
  }
}
