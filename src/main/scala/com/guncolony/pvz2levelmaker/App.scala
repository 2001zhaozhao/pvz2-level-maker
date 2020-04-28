package com.guncolony.pvz2levelmaker
import javafx.beans.value.ChangeListener
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.event.ActionEvent
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.ScrollPane.ScrollBarPolicy
import scalafx.scene.control.{Button, ContextMenu, Label, MenuItem, ScrollPane, TextArea, TextField}
import scalafx.scene.image.Image
import scalafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent}
import scalafx.scene.layout.{AnchorPane, Background, BorderPane, GridPane, HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.{Font, TextAlignment}
import scalafx.Includes._

import scala.collection.mutable.ArrayBuffer

object App extends JFXApp {
  val APP_VERSION: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("development version")

  // Current app state variables
  var currentWave: WaveEditor = null // Currently selected wave

  var textChangingInternally: Boolean = false

  val zombieListView = new GridPane {
    hgap = 6
    vgap = 4

    var zombieList: ArrayBuffer[String] = ArrayBuffer[String]()

    def addZombie(zombie: String): Unit = {
      zombieList.append(zombie)
      updatePane()
    }

    def removeZombie(zombie: String): Unit = {
      val ind = zombieList.indexOf(zombie)
      if(ind != -1) zombieList.remove(ind)
      updatePane()
    }

    def updatePane(): Unit = {
      children.clear()
      var column = 0
      var row = 0
      for(zombie <- zombieList) {
        add(new Button {
          mnemonicParsing = false
          text = zombie
          textAlignment = TextAlignment.Left
          minWidth = 120
          tooltip = "Press ` or left click to add this zombie to your wave" +
            "\nPress 1,2,3,4,5 while hovering to add this zombie to a specific lane" +
            "\n\nRight click or press DELETE/Backspace to remove this zombie from the list"

          contextMenu = new ContextMenu(new MenuItem {
            text = "Remove"
            onAction = handle{ removeZombie(zombie) }
          })

          onKeyPressed = (event:KeyEvent) => {
            event.getCode.getCode match {
              case 8 => removeZombie(zombie) // Backspace
              case 127 => removeZombie(zombie) // Delete
              case 49 => if(currentWave != null) currentWave.addZombieToLane(zombie, 1)
              case 50 => if(currentWave != null) currentWave.addZombieToLane(zombie, 2)
              case 51 => if(currentWave != null) currentWave.addZombieToLane(zombie, 3)
              case 52 => if(currentWave != null) currentWave.addZombieToLane(zombie, 4)
              case 53 => if(currentWave != null) currentWave.addZombieToLane(zombie, 5)
              case 192 => if(currentWave != null) currentWave.addZombie(zombie)
              case otherwise => {}
            }
          }

          onAction = handle {
            if(currentWave != null) currentWave.addZombie(zombie);
          }

          onMouseEntered = handle {
            requestFocus()
          }
        }, column, row)
        row += 1
        if(row >= 3) {
          column += 1
          row = 0
        }
      }
    }

    // Initial zombies
    zombieList.appendAll(Array("tutorial", "tutorial_armor1", "tutorial_armor2"))
    updatePane()
  }

  val zombieAdder = new VBox {
    padding = Insets.apply(0, 15, 0, 20)
    val zombieNameBox: TextField = new TextField {
      promptText = "Zombie Name"
      tooltip = "If the zombie is from ZombieTypes, simply type the zombie name, e.g. \"mummy\"" +
        "instead of \"RTID(mummy@ZombieTypes)\".\n" +
        "otherwise, do include the @ but do not include the RTID, like \"custom_zombie@CurrentLevel\"."
    }

    children = Seq(
      zombieNameBox,
      new Button {
        text = "Add Zombie"
        tooltip = "Type a zombie's code name into the above text field and click this button to add it to the list"
        onAction = handle {
          if(!zombieNameBox.text.value.isEmpty) {
            zombieListView.addZombie(zombieNameBox.text.value)
          }
          zombieNameBox.requestFocus()
        }
      }
    )
  }

  val jsonBox: TextArea = new TextArea {
    font = new Font("Courier New", 12)
    promptText = "Paste the JSON here and click \"Finish Editing\" to load the level"

    // When text changed, the update json button will become available
    text.addListener((_, _, _) => {
      if(!textChangingInternally) {
        Wave.hideWavesInGui()
        jsonUpdateButton.disable = false
        zombieTypesUpdateButton.disable = true
      }
    })
  }

  val jsonUpdateButton: Button = new Button{
    text = "Finish Editing"
    tooltip = "After editing the JSON manually, you must click this button to make the program reload the JSON."
    onAction = handle {
      Wave.updateModules()
      Wave.showWavesInGui()
      disable = true
      zombieTypesUpdateButton.disable = false
    }
  }

  val zombieTypesUpdateButton: Button = new Button {
    text = "Reload Zombies"
    tooltip = "Replace the zombie list on the top by a list of zombies detected in this level."
    onAction = handle {
      zombieListView.zombieList.clear()
      zombieListView.zombieList.addAll(Wave.findAllZombies())
      zombieListView.updatePane()
    }
    disable = true
  }

  val waveEditorPane = new VBox {
    margin = Insets.apply(5, 5, 5, 5)
  }

  var mainContentPane : BorderPane = null
  stage = new PrimaryStage {
    title = "Plants vs. Zombies 2 Level Maker - " + APP_VERSION
    icons.add(new Image("file:assets/icon.png"))

    width = 1280
    height = 720

    scene = new Scene {
      fill = Color.rgb(160, 167, 99)
      mainContentPane =  new BorderPane {
        hgrow = Priority.Always
        vgrow = Priority.Always
        top = new BorderPane {
          padding = Insets.apply(15, 15, 15, 15)
          style = "-fx-background-color: #b0b76d"
          center = zombieListView
          right = zombieAdder

        }

        right = new BorderPane{
          margin = Insets.apply(0, 20, 0, 0)
          center = jsonBox
          bottom = new BorderPane {
            left = jsonUpdateButton
            center = zombieTypesUpdateButton
          }
        }

        center = new ScrollPane() {
          fitToWidth = true
          style = "-fx-background: #a6ad66"
          content = waveEditorPane
        }

        // Try to fix bottom cut off
        bottom = new Label("") {
          prefHeight = 40
        }
      }
      content = mainContentPane
    }
  }

  mainContentPane.prefWidth.bind(stage.width)
  mainContentPane.prefHeight.bind(stage.height)

  jsonBox.prefWidth.bind(stage.width * 0.4)
}