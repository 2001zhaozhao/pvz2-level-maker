package com.guncolony.pvz2levelmaker

import java.util

import com.google.gson.reflect.TypeToken
import scalafx.scene.layout.{HBox, Priority, VBox}
import scalafx.Includes._
import com.google.gson.{Gson, GsonBuilder, JsonArray, JsonElement, JsonObject, JsonParser, JsonPrimitive, JsonSyntaxException}

import scala.jdk.CollectionConverters._
import scala.collection.mutable.{ArrayBuffer, HashMap}
import java.util.ArrayList

import com.guncolony.pvz2levelmaker.AmbushModules._
import scalafx.geometry.Insets
import scalafx.scene.{Node, Parent}
import scalafx.scene.control.{ButtonType, ChoiceDialog, Dialog, Label, ScrollBar, TextArea, TextField}
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

  /**
   * Adds RTID prefix and CurrentLevel suffix
   */
  def aliasToReference(alias: String): String = {
    if(alias.contains("@"))
      "RTID(" + alias + ")"
    else
      "RTID(" + alias + "@CurrentLevel)"
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

    /**
     * Sets the alias of this module, mostly used for creation of new modules
     * @return The module itself
     */
    def setAlias(alias: String): Module = {
      val aliases = new JsonArray()
      aliases.add(new JsonPrimitive(alias))
      json.add("aliases", aliases)
      this
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
  class DoubleTextField (val initialValue: Double, val valueUpdate: Double => Unit) extends TextField {
    prefWidth = 40
    text = initialValue.toString
    text.addListener((_,_,newText) => newText.toDoubleOption match {
      case Some(value) => {style = ""; valueUpdate(value)}
      case None => {style = "-fx-text-fill: red"}
    })
  }
  class StringTextField (val initialValue: String, val valueUpdate: String => Unit) extends TextField {
    prefWidth = 40
    text = initialValue.toString
    text.addListener((_,_,newText) => valueUpdate(newText))
  }

  object SpawnZombiesModule {
    // Creates a java util arraylist with 7 zeros
    def createEmptyDynamicPlantFoodList: util.ArrayList[Int] =
      new util.ArrayList[Int](util.Arrays.asList(0,0,0,0,0,0,0))

    def empty(): SpawnZombiesModule = {
      val json = new JsonObject()
      json.add("aliases", new JsonArray()) // Empty alias to make sure it's at the top of the JSON
      json.add("objclass", new JsonPrimitive("SpawnZombiesJitteredWaveActionProps"))
      val objdata = new JsonObject()
      json.add("objdata", objdata)
      objdata.add("Zombies", new JsonArray())
      new SpawnZombiesModule(json, 0, createEmptyDynamicPlantFoodList, new util.ArrayList())
    }
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
      else if(objdata.has("AdditionalPlantfood")) // Remove if existing, unless it was already 0
        if(objdata.get("AdditionalPlantfood").getAsInt > 0)
          objdata.remove("AdditionalPlantfood")

      if(dynamicPlantFood.asScala.sum > 0)
        objdata.add("DynamicPlantfood", gson.toJsonTree(dynamicPlantFood,
          new TypeToken[util.ArrayList[Integer]]{}.getType))
      else if(objdata.has("DynamicPlantfood")) // Remove if existing, unless it was already all 0
        if(gson.fromJson(objdata.get("DynamicPlantfood").getAsJsonArray, new TypeToken[util.ArrayList[Integer]]{}
            .getType).asInstanceOf[util.ArrayList[Integer]].asScala.map(_.asInstanceOf[Int]).sum > 0)
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

          classString match {
            case "SpawnZombiesJitteredWaveActionProps" =>
              // Regular wave
              val dynamicPlantFood = objdata.get("DynamicPlantfood")
              module = new SpawnZombiesModule(moduleObject,
                Option(objdata.get("AdditionalPlantfood")).getOrElse(new JsonPrimitive(0)).getAsInt,

                if (dynamicPlantFood != null) // Default value since DynamicPlantFood is optional
                  gson.fromJson(dynamicPlantFood, new TypeToken[java.util.ArrayList[Integer]]{}.getType)
                else SpawnZombiesModule.createEmptyDynamicPlantFoodList,
                // Sometimes the JSON doesn't have Zombies array, so use an Option to make sure we internally have it
                gson.fromJson(Option(objdata.get("Zombies")).getOrElse(new JsonArray()),
                new TypeToken[java.util.ArrayList[ZombieDataWithRow]]{}.getType))
            case "WaveManagerProperties" =>
              // Wave manager
              module = new WaveManagerModule(moduleObject,
                objdata.get("FlagWaveInterval").getAsInt,
                objdata.get("WaveCount").getAsInt,
                gson.fromJson(objdata.get("Waves"),
                  new TypeToken[java.util.ArrayList[java.util.ArrayList[String]]]{}.getType))
              waveManagerModule = module.asInstanceOf[WaveManagerModule]
            case "StormZombieSpawnerProps" =>
              // Sand/snow storm spawner
              module = new StormModule(moduleObject,
                objdata.get("ColumnStart").getAsInt,
                objdata.get("ColumnEnd").getAsInt,
                objdata.get("GroupSize").getAsInt,
                objdata.get("TimeBetweenGroups").getAsDouble,
                gson.fromJson(objdata.get("Zombies"), new TypeToken[java.util.ArrayList[ZombieData]]{}.getType))
            case "ParachuteRainZombieSpawnerProps" =>
              // Parachute rain
              module = new ParachuteRainModule(moduleObject,
                objdata.get("ColumnStart").getAsInt,
                objdata.get("ColumnEnd").getAsInt,
                objdata.get("GroupSize").getAsInt,
                objdata.get("SpiderCount").getAsInt,
                objdata.get("SpiderZombieName").getAsString,
                objdata.get("TimeBetweenGroups").getAsDouble)
            case "RaidingPartyZombieSpawnerProps" =>
              // Raiding party
              module = new RaidingPartyModule(moduleObject,
                objdata.get("GroupSize").getAsInt,
                objdata.get("SwashbucklerCount").getAsInt,
                objdata.get("TimeBetweenGroups").getAsDouble)
            case "BeachStageEventZombieSpawnerProps" =>
              // Low tide
              module = new LowTideModule(moduleObject,
                objdata.get("ColumnStart").getAsInt,
                objdata.get("ColumnEnd").getAsInt,
                objdata.get("GroupSize").getAsInt,
                objdata.get("ZombieCount").getAsInt,
                objdata.get("ZombieName").getAsString,
                objdata.get("TimeBetweenGroups").getAsDouble,
                objdata.get("TimeBeforeFullSpawn").getAsDouble)
            case "TidalChangeWaveActionProps" =>
              // Tidal change
              module = new TidalChangeModule(moduleObject,
                objdata.get("TidalChange").getAsJsonObject.get("ChangeAmount").getAsInt,
                objdata.get("TidalChange").getAsJsonObject.get("ChangeType").getAsString)
            case _ =>
              module = new Module(moduleObject)
          }
          modules += module

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
        waves += wavelist
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
    for(i <- waves.indices) {
      val wave = waves(i)
      val wavePane = new VBox() {
        margin = Insets.apply(10, 0, 10, 0)
        padding = Insets.apply(10, 10, 10, 10)
        style = "-fx-background-color: #b0b76d"
        children = new HBox {
          children = Seq(new Label("Wave " + (i + 1) + "  ") {style = "-fx-font-weight: bold;"},
            new Label("[+Modules]") { // Add waves button
              font = new Font("Courier New", 11)
              tooltip = "Click to add wave modules"
              onMouseEntered = handle {style = "-fx-font-weight: bold; -fx-text-fill: brown;"}
              onMouseExited = handle {style = ""}
              onMouseClicked = handle {addWaveModulesDialog("Wave" + (i + 1), module => {
                // When a module is chosen, add it to both the module list and the wave manager, and refresh the app
                if(wave.nonEmpty) {
                  // Try to insert module where it should be in the JSON file
                  if(modules.contains(wave.last)) {
                    modules.insert(modules.indexOf(wave.last) + 1, module)
                  }
                  else {
                    modules += module
                    System.err.println("Error: ArrayBuffer modules did not contain the last module of wave " + i + "!")
                  }
                }
                else {
                  modules += module
                }
                waveManagerModule.waves.get(i).add(aliasToReference(getAlias(module.json)))
                requestJsonUpdate(); App.exitTextEditMode()
              })}
            }
          )
        }
      }
      for(module <- wave) {
        wavePane.children.add(module.getDisplayNode)
      }
      App.waveEditorPane.children.add(wavePane)
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

  /**
   * Finds all zombie types in the current level
   */
  def findAllZombies(): List[String] = {
    val typeSet = scala.collection.mutable.Set[String]()
    for(moduleTypeSet <- modules.map(_.getZombieTypes)) {
      typeSet.addAll(moduleTypeSet)
    }
    typeSet.toList.sorted
  }

  // Optionally gets the module with the specified identifier
  /**
   * Optionally gets the module with the specified identifier
   */
  def getModuleWithName(name: String): Option[Module] = {
    for(module <- modules) {
      if(getAlias(module.json) == name) return Option(module)
    }
    None
  }

  /**
   * Outputs if the specified module is in any waves
   */
  def isModuleUsed(module: Module): Boolean = waves.exists(_.contains(module))

  /**
   * Adds one wave to the end of the level. It contains an empty SpawnZombiesModule.
   * <p>Uses an existing SpawnZombiesModule with the same name instead if unused.
   * If used it will attempt find a new name.
   * <p>This method edits the level JSON, so the internal storage needs to be updated after running it.
   */
  @scala.annotation.tailrec
  def addWave(usedNameIndex: Int): Unit = {
    val waveNumber: Int = waveManagerModule.waves.size + 1 // Waves start at Wave1 not Wave0
    val name: String = if(usedNameIndex == 0) "Wave"+waveNumber else "Wave"+waveNumber+"-"+usedNameIndex

    def addWaveJson(): Unit = {
      val newWave = new util.ArrayList[String]
      newWave.add(aliasToReference(name))

      waveManagerModule.waves.add(newWave)
    }
    getModuleWithName(name) match {
      case Some(module) =>
        if(isModuleUsed(module)) {
          addWave(usedNameIndex) // Find the next name instead of making two waves point to the same module
        }
        else {
          addWaveJson()
          requestJsonUpdate()
        }
      case None =>
        modules += SpawnZombiesModule.empty().setAlias(name)
        addWaveJson()
        requestJsonUpdate()
    }
  }

  /**
   * Finds the module name with the lowest number suffix (or more preferably, no suffix)
   * not occupied by an existing module.
   */
  def findAvailableModuleName(baseName: String): String = findAvailableModuleName(baseName, 0)
  @scala.annotation.tailrec
  private def findAvailableModuleName(baseName: String, usedNameIndex: Int): String = {
    val name: String = if(usedNameIndex == 0) baseName
        else if(baseName.endsWith("0")) baseName.substring(0, baseName.length - 1) + usedNameIndex
        else baseName + "-" + usedNameIndex
    getModuleWithName(name) match {
      case Some(_) => findAvailableModuleName(baseName, usedNameIndex + 1)
      case _ => name
    }
  }

  class ModuleDialogType(val displayName: String, val suffix: String, val moduleCreator: () => Module) {
    override def toString: String = displayName
  }
  val spawnZombiesType = new ModuleDialogType("Spawn Zombies", "", SpawnZombiesModule.empty)
  val moduleDialogTypes: Seq[ModuleDialogType] = Seq(
    spawnZombiesType,
    new ModuleDialogType("Sandstorm", "StormEvent0", () => emptyStormModule("sandstorm")),
    new ModuleDialogType("Snowstorm", "StormEvent0", () => emptyStormModule("snowstorm")),
    new ModuleDialogType("Parachute Rain", "ParachuteRainEvent0", emptyParachuteRainModule),
    new ModuleDialogType("Raiding Party", "RaidingPartyEvent0", emptyRaidingPartyModule),
    new ModuleDialogType("Low Tide", "LowTideEvent0", emptyLowTideModule),
    new ModuleDialogType("Tidal Change", "TidalChangeEvent0", emptyTidalChangeModule)
  )

  /**
   * Opens a dialog that allows the user to add modules to the wave
   */
  def addWaveModulesDialog(waveName: String, addModuleFunction: Module => Unit): Unit = {
    val result = new ChoiceDialog(defaultChoice = App.lastModuleType.getOrElse(spawnZombiesType),
            choices = moduleDialogTypes) {
      initOwner(App.stage)
      title = "Add Wave Module to " + waveName
      headerText = "Please choose the type of module\nto add to your wave."
      contentText = "Module type:"
    }.showAndWait()

    result match {
      case Some(moduleType) =>
        App.lastModuleType = Some(moduleType)
        // Run the function to add this module to the wave
        addModuleFunction(moduleType.moduleCreator().setAlias(findAvailableModuleName(waveName + moduleType.suffix)))
      case None =>
    }
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
