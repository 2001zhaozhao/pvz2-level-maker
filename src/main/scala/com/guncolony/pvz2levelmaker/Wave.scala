package com.guncolony.pvz2levelmaker

import java.util

import com.google.gson.reflect.TypeToken
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.Includes._
import com.google.gson.{Gson, GsonBuilder, JsonArray, JsonElement, JsonObject, JsonParser, JsonPrimitive, JsonSyntaxException}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.util.ArrayList

import scalafx.geometry.Insets
import scalafx.scene.{Node, Parent}
import scalafx.scene.control.{Label, ScrollBar, TextArea, TextField}
import scalafx.scene.text.{Font, Text}

import scala.collection.mutable

object Wave {
  // This object parses the level file in the jsonBox and splits it into modules
  val gson:Gson = new GsonBuilder().setPrettyPrinting().create()

  // Helper methods
  /**
   * Converts the original zombie name to a more user friendly version. It will remove the RTID and if the zombie is
   * from ZombieTypes, it will also remove the @ZombieTypes
   */
  def toFriendlyZombieName(originalName: String): String = {
    val withoutRTID = originalName.substring(5, originalName.length - 1)
    if(withoutRTID.endsWith("@ZombieTypes"))
      withoutRTID.substring(0, withoutRTID.length - 12)
    else
      withoutRTID
  }

  /**
   * Removes the RTID( prefix and @CurrentLevel) suffix. Example: RTID(Wave1@CurrentLevel) -> Wave1
   */
  def referenceToAlias(originalName: String): String = {
    val withoutRTID = originalName.substring(5, originalName.length - 1)
    if(withoutRTID.endsWith("@CurrentLevel"))
      withoutRTID.substring(0, withoutRTID.length - 13)
    else
      withoutRTID
  }

  /**
   * Converts the user-friendly zombie name to the original used in files. Adds @ZombieTypes if missing the @ sign,
   * and adds RTID
   */
  def toOriginalZombieName(friendlyName: String): String = {
    if(friendlyName.contains("@"))
      "RTID(" + friendlyName + ")"
    else
      "RTID(" + friendlyName + "@ZombieTypes)"
  }

  val aliasesUnnamed = new JsonArray()
  aliasesUnnamed.add("[unnamed]")
  def getAlias(json: JsonObject): String =
    Option(json.get("aliases")).getOrElse(aliasesUnnamed).getAsJsonArray.get(0).getAsString

  // Classes to lay out the format of the level file
  class Module (val json: JsonObject) {
    /**
     * Gets the current JSON that this module serializes to, after any edits done to it
     */
    def getCurrentJson: JsonObject = json

    override def toString: String = "Module - " + getCurrentJson.toString

    /**
     * Gets a set of the zombie types (friendly name) that appear in this wave
     */
    def getZombieTypes: Set[String] = Set[String]()

    /**
     * Gets a Node to display onto the main GUI which allows viewing/editing this module.
     * @return
     */
    def getDisplayNode: Node = {
      new VBox {
        val editor = new TextArea {
          text = gson.toJson(json.get("objdata"))
          font = new Font("Courier New", 12)

          text.addListener((_,_,newText) => {
            try {
              val jsonOfText: JsonObject = gson.fromJson(newText, classOf[JsonObject])
              json.add("objdata", jsonOfText)
              requestJsonUpdate()
              style = ""
            } catch {
              case e: JsonSyntaxException => style = "-fx-text-fill: red"
            }
          })
        }

        children = Seq(new Label(getAlias(json) + " - " + json.get("objclass").getAsString), editor)
      }
    }
  }
  class WaveManagerModule (override val json: JsonObject, var flagWaveInterval: Int, var waveCount: Int,
                           var waves: java.util.ArrayList[java.util.ArrayList[String]]) extends Module (json) {

    override def toString: String = "WaveManagerModule - flagWaveInterval = " + flagWaveInterval +
        ", waveCount = " + waveCount + ", waves: " + waves

    override def getCurrentJson: JsonObject = {
      // Note - this has the side effect of changing the relevant fields in variable--json
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      objdata.add("FlagWaveInterval", new JsonPrimitive(flagWaveInterval))
      objdata.add("WaveCount", new JsonPrimitive(waveCount))
      objdata.add("Waves", gson.toJsonTree(waves,
        new TypeToken[util.ArrayList[util.ArrayList[String]]]{}.getType))
      json
    }
  }

  /**
   * Gets the appropriate number of tabs to put after a zombie name in the zombie list textboxes
   */
  def getTabSeparatorCount(stringLength: Int): Int = {
    var tabs = ((24 - (stringLength % 24) + 7) / 8)
    if(tabs == 0) tabs = 3 // This covers the case when the zombie name is exactly length 24,48 etc
    tabs
  }

  object ZombieDataWithRow {
    def fromString(string: String): ZombieDataWithRow = {
      val str = string.replace('-', '_')
      if (str.charAt(0) >= '0' && str.charAt(0) <= '9')
        if (str.length >= 2)
          new ZombieDataWithRow(str.substring(0, 1), toOriginalZombieName(str.substring(2)))
        else
          new ZombieDataWithRow(str.substring(0, 1), "")
      else
        new ZombieDataWithRow(null, toOriginalZombieName(str))
    }

    def getZombieTextStr(Zombies: Iterable[String]): String = {
      val builder = new StringBuilder()
      for(zombie <- Zombies) {
        // Append tabs to make 16 characters
        builder.append(zombie + "\u0009" * getTabSeparatorCount(zombie.length))
      }
      builder.toString.trim
    }
    def getZombieText(Zombies: Iterable[ZombieDataWithRow]): String
      = getZombieTextStr(Zombies.map(_.toString))
    def updateFromZombieText(text: String): java.util.ArrayList[ZombieDataWithRow] = {
      val Zombies = new java.util.ArrayList[ZombieDataWithRow]
      text.trim().split("\\s+").map(text => Zombies.add(ZombieDataWithRow.fromString(text)))
      Zombies
    }
  }
  class ZombieDataWithRow (Row: String, Type: String) {
    override def toString: String =
      if (Row != null) Row + "_" + toFriendlyZombieName(Type) else toFriendlyZombieName(Type)
    def Type_(): String = Type
  }
  object ZombieData {
    def fromString(str: String): ZombieData = new ZombieData(toOriginalZombieName(str))

    def getZombieText(Zombies: Iterable[ZombieData]): String
      = ZombieDataWithRow.getZombieTextStr(Zombies.map(_.toString))
    def updateFromZombieText(text: String): java.util.ArrayList[ZombieData] = {
      val Zombies = new java.util.ArrayList[ZombieData]
      text.trim().split("\\s+").map(text => Zombies.add(ZombieData.fromString(text)))
      Zombies
    }
  }
  class ZombieData (Type: String) {
    override def toString: String = toFriendlyZombieName(Type)
    def Type_(): String = Type
  }
  class IntTextField (val initialValue: Int, val valueUpdate: Int => Unit) extends TextField {
    prefWidth = 40
    text = initialValue.toString
    text.addListener((_,_,newText) => newText.toIntOption match {
      case Some(value) => {style = ""; valueUpdate(value)}
      case None => {style = "-fx-text-fill: red"}
    })
  }

  class SpawnZombiesModule (override val json: JsonObject, var additionalPlantFood: Int,
                            var dynamicPlantFood: java.util.ArrayList[Int],
                            var Zombies: java.util.ArrayList[ZombieDataWithRow]) extends Module (json) {
    override def toString: String = "SpawnZombiesModule - " + Zombies.toString
    override def getZombieTypes: Set[String] = Zombies.asScala.map(_.Type_()).map(toFriendlyZombieName).toSet

    override def getCurrentJson: JsonObject = {
      // Note - this has the side effect of changing the relevant fields in variable--json
      val objdata: JsonObject = json.get("objdata").getAsJsonObject
      if(additionalPlantFood > 0)
        objdata.add("AdditionalPlantfood", new JsonPrimitive(additionalPlantFood))
      else
        objdata.remove("AdditionalPlantfood")
      if(dynamicPlantFood.asScala.sum > 0)
        objdata.add("DynamicPlantfood", gson.toJsonTree(dynamicPlantFood,
          new TypeToken[util.ArrayList[Int]]{}.getType))
      else
        objdata.remove("DynamicPlantfood")
      objdata.add("Zombies", gson.toJsonTree(Zombies,
        new TypeToken[util.ArrayList[ZombieDataWithRow]]{}.getType))
      json
    }


    override def getDisplayNode: Node = {
      new VBox {
        val editor: WaveEditor = new WaveEditor {
          override val acceptLanes = true

          text = ZombieDataWithRow.getZombieText(Zombies.asScala)
          font = new Font("Courier New", 13)
          wrapText = true
          prefRowCount = 4
          text

          text.addListener((_,_,newText) => {
            Zombies = ZombieDataWithRow.updateFromZombieText(newText)
            requestJsonUpdate()
          })
        }

        children = Seq(new HBox {
            children = Seq(new Label(getAlias(json) + " - Spawn Zombies | PF: "),
              new IntTextField(additionalPlantFood, v => {additionalPlantFood = v; requestJsonUpdate()}),
              new Label(" Dynamic PF: "),
              new IntTextField(dynamicPlantFood.get(0), v => {dynamicPlantFood.set(0, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(1), v => {dynamicPlantFood.set(1, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(2), v => {dynamicPlantFood.set(2, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(3), v => {dynamicPlantFood.set(3, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(4), v => {dynamicPlantFood.set(4, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(5), v => {dynamicPlantFood.set(5, v); requestJsonUpdate()}),
              new IntTextField(dynamicPlantFood.get(6), v => {dynamicPlantFood.set(6, v); requestJsonUpdate()}),
            )
        }, editor)
      }
    }
  }
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

  // List of modules
  var modules: ArrayBuffer[Module] = new ArrayBuffer[Module]()
  var moduleAliasMap: mutable.HashMap[String, Module] = new mutable.HashMap[String, Module]()
  var waves: ArrayBuffer[ArrayBuffer[Module]] = new ArrayBuffer[ArrayBuffer[Module]]()
  var waveManagerModule: WaveManagerModule = null

  def getCurrentJson: String = App.jsonBox.text.value

  /**
   * Parse modules and waves from the current file
   */
  def updateModules(): Unit = {
    modules.clear()
    waves.clear()
    waveManagerModule = null

    // Reset scrolling (there is a bug where the scrolling resets once, do it now so that it wont reset later)
    App.jsonBox.text = App.jsonBox.text.value

    val jsonElement: JsonElement = new JsonParser().parse(getCurrentJson)
    if(!jsonElement.isJsonObject) {
      println("Invalid JSON file!")
      return
    }
    val fileObject: JsonObject = jsonElement.getAsJsonObject
    val objects: JsonArray = fileObject.getAsJsonArray("objects")
    for(element: JsonElement <- objects.asScala) {
      if(element.isJsonObject) {
        val moduleObject = element.getAsJsonObject
        // Adds model based on the type of module defined in the file
        val objclass = moduleObject.get("objclass")
        if(objclass.isJsonPrimitive) {
          val classString = objclass.getAsString
          var module: Module = null

          val objdataElement = moduleObject.get("objdata")
          val objdata = if (objdataElement != null) objdataElement.getAsJsonObject else null
          if(classString == "SpawnZombiesJitteredWaveActionProps") {
            // Regular wave
            val dynamicPlantFood = objdata.get("DynamicPlantfood")
            module = new SpawnZombiesModule(moduleObject,
              Option(objdata.get("AdditionalPlantfood")).getOrElse(new JsonPrimitive(0)).getAsInt,

              if (dynamicPlantFood != null) // Default value since DynamicPlantFood is optional
                gson.fromJson(dynamicPlantFood, new TypeToken[java.util.ArrayList[Int]]{}.getType)
              else new util.ArrayList[Int](util.Arrays.asList(0,0,0,0,0,0,0)),

              gson.fromJson(objdata.get("Zombies"), new TypeToken[java.util.ArrayList[ZombieDataWithRow]]{}.getType))
          }
          else if(classString == "StormZombieSpawnerProps") {
            // Sand/snow storm spawner
            module = new StormModule(moduleObject,
              objdata.get("ColumnStart").getAsInt,
              objdata.get("ColumnEnd").getAsInt,
              objdata.get("GroupSize").getAsInt,
              objdata.get("TimeBetweenGroups").getAsInt,
              gson.fromJson(objdata.get("Zombies"), new TypeToken[java.util.ArrayList[ZombieData]]{}.getType))
          }
          else if(classString == "WaveManagerProperties") {
            // Wave manager
            module = new WaveManagerModule(moduleObject,
              objdata.get("FlagWaveInterval").getAsInt,
              objdata.get("WaveCount").getAsInt,
              gson.fromJson(objdata.get("Waves"),
                new TypeToken[java.util.ArrayList[java.util.ArrayList[String]]]{}.getType))
            waveManagerModule = module.asInstanceOf[WaveManagerModule]
          }
          else {
            module = new Module(moduleObject)
          }
          modules.addOne(module)

          // Store alias in a map for parsing waves later
          val aliases = moduleObject.get("aliases")
          if(aliases != null) {
            for(element <- aliases.getAsJsonArray.asScala) {
              moduleAliasMap.put(element.getAsString, module)
            }
          }

        }
      }
    }

    // Put the modules into waves
    if(waveManagerModule != null) {
      for(wave: mutable.Buffer[String] <- waveManagerModule.waves.asScala.map(_.asScala)) {
        val wavelist = new ArrayBuffer[Module]()
        // Add each wave to wavelist if it is a valid wave module
        wave.map(referenceToAlias).map(str => moduleAliasMap.get(str).map(wavelist.addOne))
        waves.addOne(wavelist)
      }
    }

    // Debug
    /*
    println(modules)
    println(moduleAliasMap)
    println(waves)
     */

    // Add the visual representation of waves to the main GUI
    App.waveEditorPane.children.clear()
    var index = 0
    for(wave <- waves) {
      val wavePane = new VBox() {
        margin = Insets.apply(10, 0, 10, 0)
        padding = Insets.apply(10, 10, 10, 10)
        style = "-fx-background-color: #b0b76d"
        children = new Label("Wave " + (index+1)) {
          style = "-fx-font-weight: bold;"
        }
      }
      for(module <- wave) {
        wavePane.children.add(module.getDisplayNode)
      }
      App.waveEditorPane.children.add(wavePane)
      index += 1
    }
  }

  // Below are functions that are called from GUI buttons

  /**
   * Requests to update the JSON in the text box based on the current values of the modules.
   */
  def requestJsonUpdate(): Unit = {
    //Current functionality just updates the JSON immediately. This will not give the best performance.
    // The plan is to make this multi threaded later

    // Load the JSON file
    val jsonElement: JsonElement = new JsonParser().parse(getCurrentJson)
    if(!jsonElement.isJsonObject) {
      println("Invalid JSON file!")
      return
    }
    val fileObject: JsonObject = jsonElement.getAsJsonObject

    // Replace "objects" with the current values of the modules

    val newModules = new JsonArray()
    for(module <- modules) {
      newModules.add(module.getCurrentJson)
    }

    /*
    val array = fileObject.get("objects").getAsJsonArray
    while(array.size > 0) {
      array.remove(array.size - 1)
    }
    */

    fileObject.add("objects", newModules)

    val prettyJsonString = gson.toJson(fileObject)

    // The default indent is 2 spaces, we need to double the leading spaces
    val builder = new StringBuilder()
    var afterNewline = true
    for(char <- prettyJsonString) {
      if(char == '\n') {
        afterNewline = true
        builder.append(char)
      }
      else if(char == ' ' && afterNewline) {
        builder.append("  ")
      }
      else {
        afterNewline = false
        builder.append(char)
      }
    }

    // Put the text into the text box
    val hscroll = App.jsonBox.scrollLeft.value
    val vscroll = App.jsonBox.scrollTop.value
    App.textChangingInternally = true
    App.jsonBox.text = builder.toString
    App.textChangingInternally = false
    App.jsonBox.scrollLeft = hscroll
    App.jsonBox.scrollTop = vscroll
  }

  // Hide/show required because after editing text box the waves will be hidden until you click update
  def hideWavesInGui(): Unit = {
    App.waveEditorPane.children.clear()
    App.currentWave = null
  }

  def showWavesInGui(): Unit = {

  }

  // Finds all zombie types in the current level
  def findAllZombies(): List[String] = {
    val typeSet = scala.collection.mutable.Set[String]()
    for(moduleTypeSet <- modules.map(_.getZombieTypes)) {
      typeSet.addAll(moduleTypeSet)
    }
    typeSet.toList.sorted
  }
}

class WaveEditor extends TextArea {
  // Overridable
  val acceptLanes = false

  def addZombie(zombie: String): Unit = {
    val addPosition = text.value.length //TBI using current cursor as add position

    val before = text.value.substring(0, addPosition).trim
    val after = text.value.substring(addPosition, text.value.length)

    val builder = new StringBuilder(before)

    // (Re)Insert tabs after the previous zombie
    val split = before.split("\\s+")
    if(split.nonEmpty) {
      val lastLength = split(split.length - 1).length
      builder.append("\u0009" * Wave.getTabSeparatorCount(lastLength))
    }

    builder.append(zombie)
    // Insert tabs after the zombie, if there is still something afterwards
    if(after.trim.nonEmpty) {
      builder.append("\u0009" * Wave.getTabSeparatorCount(zombie.length))
    }
    builder.append(after)
    text = builder.toString
  }

  def addZombieToLane(zombie: String, lane: Int): Unit =
    if(acceptLanes) addZombie(lane + "_" + zombie) else addZombie(zombie)

  onMouseClicked = handle {
    // Change the currently active wave editor
    if(App.currentWave != null) App.currentWave.style = ""
    App.currentWave = this
    style = "-fx-background-color: brown"
  }
}
