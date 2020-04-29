package com.guncolony.pvz2levelmaker

import java.util

import com.google.gson.{JsonObject, JsonPrimitive}
import com.google.gson.reflect.TypeToken
import com.guncolony.pvz2levelmaker.Wave.{IntTextField, Module, ZombieData, getAlias, gson, requestJsonUpdate, toFriendlyZombieName}
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
  class StormModule (override val json: JsonObject, var columnStart: Int, var columnEnd: Int, var groupSize: Int,
                     var timeBetweenGroups: Int, var Zombies: java.util.ArrayList[ZombieData]) extends Module (json) {
    override def toString: String = "StormModule - " + Zombies.toString
    override def getZombieTypes: Set[String] = Zombies.asScala.map(_.Type_()).map(toFriendlyZombieName).toSet

    override def getCurrentJson: JsonObject = {
      // Note - this has the side effect of changing the relevant fields in variable--json
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("ColumnStart", new JsonPrimitive(columnStart))
      objdata.add("ColumnEnd", new JsonPrimitive(columnEnd))
      objdata.add("GroupSize", new JsonPrimitive(groupSize))
      objdata.add("TimeBetweenGroups", new JsonPrimitive(timeBetweenGroups))
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
            new Label(" Seconds Between Groups: "),
            new IntTextField(timeBetweenGroups, v => {timeBetweenGroups = v; requestJsonUpdate()}),
          )
        }, editor)
      }
    }
  }

}
