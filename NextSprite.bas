$Debug
Option _Explicit

Dim f&
Screen _NewImage(640, 480, 32) ' 800x600 window, 32-bit color
_FullScreen
_Title "ZX Spectrum Next Sprite/Tile/Tilemap Editor IDE"
_MouseShow ' Show mouse cursor
f& = _LoadFont("/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf", 11, "MONOSPACE,DONTBLEND")
_Font f&

' Main variables
Dim Shared As Integer activeMenu
Dim Shared As String activeKey
Dim Shared As _Unsigned Long colorWhite, colorBlack

' color table, each 0-511 element has the rrr,ggg,bbb component set
Dim Shared ColorTable(511, 3) As Integer
Dim Shared ColorValue(511) As _Unsigned Long
Dim Shared As Integer sel512Colour
Dim Shared As Integer canvasColour

' allow for 128 tiles/sprites, in reality 64 sprites, 128 tiles
' 0 = sprite
' 1 = tile
Dim Shared sprites(1, 127, 255) As Integer
Dim Shared clipBoard(63, 63) As Integer
Dim Shared tilemapClipboard(1279) As Integer
Dim Shared tempSprite(63, 63) As Integer
Dim Shared As Integer clipBoardhasData, clipBoardSource

' regular 40x32 tilemap
Dim Shared tilemapPages(10, 1279) As Integer

' allow 4 palettes, index1 is editMode, index2 is paletteselected
Dim Shared palettes(1, 1, 255) As Integer
Dim Shared As Integer paletteSelected

' 0 = Sprite
' 1 = Tile
' 2 = TileMap
Dim Shared As Integer editMode, editAssetType

Dim Shared As String fileName, fileDrive
Dim Shared As Integer boxPage, maxPage, canvasIndex, tilemapPage

Dim Shared As Integer gridXSize, gridYSize, spriteXGridSize, spriteYGridSize, tileXGridSize, tileYGridsize
Dim Shared As Integer tilemapGridlx, tilemapGridhx, tilemapGridly, tilemapGridhy
Dim Shared As Integer palettelX, palettehX, palettelY, palettehY
Dim Shared As Integer colour512lX, colour512hX, colour512lY, colour512hY
Dim Shared As Integer canvaslX, canvashX, canvaslY, canvashY

Dim Shared As Integer bg1(16, 388)
Dim Shared As Integer tilePainted

colorWhite = _RGB(255, 255, 255)
colorBlack = _RGB(0, 0, 0)

Const CELL_SIZE_PIXELS = 8
Const NOT_SET = 999
Const EDIT_SPRITE = 0
Const EDIT_TILE = 1
Const EDIT_TILEMAP = 2
Const menu_Base = 0
Const menu_File = 1
Const menu_Edit = 2
Const menu_editMode = 3
Const menu_GridSize = 4
Const menu_Generate = 5
Const menu_Import = 6
Const MAX_SPRITES = 64
Const MAX_TILES = 128
Const MAX_TILEMAP_PAGES = 10
Const boxesPerPage = 32
Const generate_raw = 0
Const generate_rle = 1

Dim Shared As String projectAmended

' max 10 menu items, 5 menu types, each with lX, lY, hX, hY parameters
Dim Shared As Integer menuData(10, 6, 2, 4)

' 5 menu types with max 10 menu options
Dim Shared As String menuText(6, 10, 2)

' box positional data
Dim Shared As Integer boxdata(32, 4)


Dim Shared As Long byteswritten, orgStart
'
' START OF PROGRAM
'

' load the menu system data
loadMenuData

' create the 512 colour table arrays
setupColorTable

' initialise stuff
Call NewProject
Call initialDraw
canvasIndex = 0
boxPage = 0
projectAmended = "N"

' Main loop
Do

    ' Draw submenu only if its parent is active
    If activeMenu > menu_Base Then
        DrawMenu (activeMenu)
    End If

    ' Handle mouse input
    While _MouseInput
        If _MouseButton(1) Then
            HandleButton_1_Click _MouseX, _MouseY
        End If
        If _MouseButton(2) Then
            HandleButton_2_Click _MouseX, _MouseY
        End If
    Wend

    activeKey = UCase$(InKey$)
    If activeKey <> "" Then
        Call handleActiveKey
    End If

    _Display
    _Limit 60
Loop Until activeKey = Chr$(27) ' Exit on ESC
System

Sub initialDraw
    Cls
    activeMenu = menu_Base

    Select Case editMode
        Case EDIT_SPRITE
            _PrintString (10, 0), "Sprite Edit"
            Call drawPaletteGrid
            Call draw512ColourGrid
            Call drawCanvasGrid
            Call drawPreviewBoxes
            Call drawCommandKeys
            Call paintPreviewBoxes
        Case EDIT_TILE
            _PrintString (10, 0), "Tile Edit"
            Call drawPaletteGrid
            Call draw512ColourGrid
            Call drawCanvasGrid
            Call drawPreviewBoxes
            Call drawCommandKeys
            Call paintPreviewBoxes
        Case EDIT_TILEMAP
            ' title is displzayed in drawTilemapGrid function due to page number
            canvasIndex = 0
            Call drawTilemapGrid
            Call drawPreviewBoxes
            Call drawCommandKeys
            Call paintPreviewBoxes
    End Select

    ' Always draw main menu
    DrawMenu (activeMenu)
    If fileName = "" Then
        _PrintString (10, 460), "untitled"
    Else
        _PrintString (10, 480 - 20), fileDrive + fileName + ".stt"
    End If
End Sub

Sub drawCommandKeys
    Dim As Integer x, y
    Dim As String ckeys
    x = boxdata(0, 1)
    y = boxdata(0, 2)

    If editMode <> EDIT_TILEMAP Then
        y = canvaslY
        x = x - 162
        ckeys = Chr$(26) + Chr$(27) + Chr$(24) + Chr$(25)

        _PrintString (x, y), "[" + ckeys + "] Shift 1 pix"
        _PrintString (x, y + 15), "[H]    Flip Horizontal"
        _PrintString (x, y + 30), "[V]    Flip Vertical"
        _PrintString (x, y + 60), "[PgDn] Prev page"
        _PrintString (x, y + 75), "[PgUp] Next page"
    Else
        _PrintString (x, y + 270), "[PgDn] Prev tile page"
        _PrintString (x, y + 285), "[PgUp] Next tile page"
        _PrintString (x, y + 300), "[" + Chr$(24) + "]" + " Prev TileMap page"
        _PrintString (x, y + 315), "[" + Chr$(25) + "]" + " Next TileMap page"

    End If
End Sub


Sub DrawMenu (activeMenu As Integer)
    Dim As Integer iC, lX, lY, hX, hY, tX, tY, c

    c = colorWhite

    ' extract each menu item from the array, draw the box and display the menu text
    iC = 1
    Do While menuData(iC, activeMenu, editMode, 1) <> 0
        lX = menuData(iC, activeMenu, editMode, 1)
        lY = menuData(iC, activeMenu, editMode, 2)
        hX = menuData(iC, activeMenu, editMode, 3)
        hY = menuData(iC, activeMenu, editMode, 4)
        tX = lX + 5
        tY = lY + 5
        Line (lX, lY)-(hX, hY), c, BF
        _PrintString (tX, tY), menuText(activeMenu, iC, editMode)
        iC = iC + 1
    Loop

End Sub

Sub drawCanvasGrid ()
    Dim As Integer ix, iy, x1, y1, x2, y2, l1, l2
    ix = 10
    iy = 50

    If editMode = EDIT_SPRITE Then
        gridXSize = spriteXGridSize
        gridYSize = spriteYGridSize
    Else
        gridXSize = tileXGridSize
        gridYSize = tileYGridsize
    End If

    x1 = ix
    y1 = iy

    For l2 = 0 To gridYSize - 1
        For l1 = 0 To gridXSize - 1
            x2 = x1 + CELL_SIZE_PIXELS - 1
            y2 = y1 + CELL_SIZE_PIXELS - 1
            Line (x1, y1)-(x2, y2), colorWhite, B

            Line (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), getSpriteColour(canvasIndex, l1, l2), BF
            x1 = x1 + CELL_SIZE_PIXELS
        Next l1

        x1 = ix
        y1 = y1 + CELL_SIZE_PIXELS
    Next l2

    canvaslX = ix
    canvaslY = iy
    canvashX = x2
    canvashY = y2
End Sub


Sub drawPaletteGrid ()
    Dim As Integer ix, iy, x, y, x1, y1, x2, y2, l1, l2, z, c
    ix = 10
    iy = 330
    x = 64
    y = 4
    x1 = ix
    y1 = iy

    z = 0
    For l2 = 1 To y
        For l1 = 1 To x
            x2 = x1 + CELL_SIZE_PIXELS - 1
            y2 = y1 + CELL_SIZE_PIXELS - 1

            Line (x1, y1)-(x2, y2), colorWhite, B
            If palettes(editMode, paletteSelected, z) <> NOT_SET Then
                Line (x1 + 1, y1 + 1)-(x2 - 1, y2 - 1), ColorValue(palettes(editMode, paletteSelected, z)), BF
            End If
            z = z + 1
            x1 = x1 + CELL_SIZE_PIXELS
        Next l1

        x1 = ix
        y1 = y1 + CELL_SIZE_PIXELS
    Next l2
    Select Case editMode
        Case EDIT_SPRITE
            _PrintString (30, iy - 12), "Sprite Palette #" + lead0(paletteSelected, 1)
        Case EDIT_TILE
            _PrintString (30, iy - 12), "Tile Palette #" + lead0(paletteSelected, 1)
    End Select

    ' palette #0 is the transparent colour
    _PrintString (10, iy - 12), "T"

    Line (530, iy)-(530 + CELL_SIZE_PIXELS, iy + CELL_SIZE_PIXELS), colorWhite, B
    If canvasColour <> 0 Then
        Line (531, iy + 1)-(531 + CELL_SIZE_PIXELS - 2, iy + 1 + CELL_SIZE_PIXELS - 2), ColorValue(palettes(editMode, paletteSelected, canvasColour)), BF
        c = palettes(editMode, paletteSelected, canvasColour)
        _PrintString (545, iy), lead0(c, 3)
        _PrintString (545, iy + 11), lead0(ColorTable(c, 1), 3) + " " + lead0(ColorTable(c, 2), 3) + " " + lead0(ColorTable(c, 3), 3)
    Else
        _PrintString (545, iy), "not set"
        _PrintString (545, iy + 11), String$(11, " ")
    End If

    palettelX = ix
    palettelY = iy
    palettehX = x2
    palettehY = y2
End Sub

Sub draw512ColourGrid ()
    Dim As Integer ix, iy, x1, y1, x2, y2, l1, l2, z

    ix = 10
    iy = 395

    x1 = ix
    y1 = iy
    z = 0
    For l2 = 1 To 8
        For l1 = 1 To 64
            x2 = x1 + CELL_SIZE_PIXELS - 1
            y2 = y1 + CELL_SIZE_PIXELS - 1
            Line (x1, y1)-(x2, y2), ColorValue(z), BF

            x1 = x1 + CELL_SIZE_PIXELS
            z = z + 1
        Next l1

        x1 = ix
        y1 = y1 + CELL_SIZE_PIXELS
    Next l2

    Line (530, iy)-(530 + CELL_SIZE_PIXELS, iy + CELL_SIZE_PIXELS), colorWhite, B
    Line (531, iy + 1)-(531 + CELL_SIZE_PIXELS - 2, iy + 1 + CELL_SIZE_PIXELS - 2), ColorValue(sel512Colour), BF
    _PrintString (545, iy), lead0(sel512Colour, 3)
    _PrintString (545, iy + 11), lead0(ColorTable(sel512Colour, 1), 3) + " " + lead0(ColorTable(sel512Colour, 2), 3) + " " + lead0(ColorTable(sel512Colour, 3), 3)

    _PrintString (ix, iy - 16), "512 Palette Choice"

    colour512lX = ix
    colour512lY = iy
    colour512hX = x2
    colour512hY = y2
End Sub

Sub drawTilemapGrid ()
    Dim As Integer ix, iy, x1, y1, x2, y2, l1, l2, pix

    ix = 10
    iy = 50

    pix = getWidth
    tilePainted = NOT_SET

    x1 = ix
    y1 = iy
    For l2 = 0 To 31
        For l1 = 0 To 39
            x2 = x1 + pix + 1
            y2 = y1 + pix + 1
            Line (x1, y1)-(x2, y2), colorWhite, B
            Call paint1Tile(tilemapPages(tilemapPage, (l2 * 40) + l1))
            Put (x1 + 1, y1 + 1), bg1(), PSet

            x1 = x1 + pix + 1
        Next l1

        x1 = ix
        y1 = y1 + pix + 1
    Next l2

    tilemapGridlx = ix
    tilemapGridly = iy
    tilemapGridhx = x2 - 1
    tilemapGridhy = y2 - 1
    _PrintString (10, 0), "TileMap Edit page : " + lead0(tilemapPage, 2)

End Sub

Sub drawPreviewBoxes ()
    Dim As Integer x, y, ix, iy, pix, hSpacer, vSpacer, b, box, c, canvasindex1

    ix = 440
    iy = 55
    hSpacer = 35
    vSpacer = 35

    pix = getWidth + 1

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            canvasindex1 = getSpriteIndex(canvasIndex, gridXSize - 1, gridYSize - 1)
        Case Else
            canvasindex1 = canvasIndex
    End Select

    x = ix
    y = iy
    c = 1
    box = 0

    For b = (boxPage * boxesPerPage) To ((boxPage * boxesPerPage) + boxesPerPage - 1)
        Line (x, y)-(x + pix, y + pix), colorWhite, B
        boxdata(box, 1) = x
        boxdata(box, 2) = y
        boxdata(box, 3) = x + pix
        boxdata(box, 4) = y + pix

        If b >= canvasIndex And b <= canvasindex1 Then
            Color colorBlack, colorWhite
        Else
            Color colorWhite, colorBlack
        End If
        _PrintString (x, y - 12), lead0(b, 3)

        x = x + hSpacer
        c = c + 1
        box = box + 1
        If c = 5 Then
            x = ix
            y = y + vSpacer
            c = 1
        End If
    Next b
    Color colorWhite, colorBlack
End Sub

Sub paintPreviewBoxes
    Dim As Integer b, lx, ly, z, x, y
    Dim As _Unsigned Long c

    z = getWidth - 1

    For b = (boxPage * boxesPerPage) To ((boxPage * boxesPerPage) + boxesPerPage - 1)
        lx = boxdata(b Mod boxesPerPage, 1) + 1
        ly = boxdata(b Mod boxesPerPage, 2) + 1
        For x = 0 To z
            For y = 0 To z
                c = getSpriteColour(b, x, y)
                PSet (lx + x, ly + y), c
            Next y
        Next x
    Next b
End Sub

Sub paint1Tile (tileindex As Integer)
    Dim As _Unsigned Long c
    Dim As Integer x1, y1, x, y, c1, z

    If tileindex = tilePainted Then Exit Sub

    x1 = 100
    y1 = 400
    x = 0
    y = 0
    For z = 0 To 63
        c1 = sprites(editAssetType, tileindex, z)
        c = ColorValue(palettes(editAssetType, paletteSelected, c1))
        PSet (x + x1, y + y1), c
        x = x + 1
        If x > 7 Then
            x = 0
            y = y + 1
        End If
    Next z
    Get (100, 400)-(107, 407), bg1()
    tilePainted = tileindex
End Sub

Sub updatePreviewBoxes (canvasIndex As Integer)
    Dim As Integer b, lx, ly, z, x, y, canvasindex1
    Dim As _Unsigned Long c

    z = getWidth - 1

    canvasindex1 = getSpriteIndex(canvasIndex, gridXSize - 1, gridYSize - 1)


    For b = canvasIndex To canvasindex1
        lx = boxdata(b Mod boxesPerPage, 1) + 1
        ly = boxdata(b Mod boxesPerPage, 2) + 1

        If b >= (boxPage * boxesPerPage) And b <= ((boxPage * boxesPerPage) + boxesPerPage - 1) Then
            For x = 0 To z
                For y = 0 To z
                    c = getSpriteColour(b, x, y)
                    PSet (lx + x, ly + y), c
                Next y
            Next x
        End If
    Next b
End Sub


Sub handleMenuClick (activemenu As Integer, menuSelected As String)
    Select Case activemenu
        Case menu_Base
            Select Case menuSelected
                Case "File ..."
                    activemenu = menu_File
                Case "Edit"
                    activemenu = menu_Edit
                Case "Edit Type"
                    activemenu = menu_editMode
                Case "Grid Size"
                    activemenu = menu_GridSize
                Case "Generate"
                    activemenu = menu_Generate
                Case "Import"
                    activemenu = menu_Import
            End Select

        Case menu_File
            Select Case menuSelected
                Case "New ..."
                    Call NewProject
                Case "Load ..."
                    Call LoadProject
                Case "Save ..."
                    Call SaveProject
            End Select
            activemenu = menu_Base
            Call initialDraw

        Case menu_Edit
            Select Case menuSelected
                Case "Copy"
                    Call clipBoardCopy(editMode)
                Case "Cut"
                    Call clipBoardCut(editMode)
                Case "Paste"
                    Call clipBoardPaste(editMode)
            End Select
            activemenu = menu_Base
            Call initialDraw

        Case menu_editMode
            Select Case menuSelected
                Case "Sprite"
                    editMode = EDIT_SPRITE
                    editAssetType = EDIT_SPRITE
                    maxPage = (MAX_SPRITES / boxesPerPage) - 1
                    If boxPage > maxPage Then boxPage = maxPage
                Case "Tile"
                    editMode = EDIT_TILE
                    editAssetType = EDIT_TILE
                    maxPage = (MAX_TILES / boxesPerPage) - 1
                    If boxPage > maxPage Then boxPage = maxPage
                Case "TileMap"
                    editMode = EDIT_TILEMAP
                    editAssetType = EDIT_TILE
                    maxPage = (MAX_TILES / boxesPerPage) - 1
                    canvasIndex = 0
                    boxPage = 0
                    tilePainted = NOT_SET
                    clipBoardhasData = 0
            End Select
            canvasColour = 0
            'clipBoardhasData = 0
            Call initialDraw

        Case menu_GridSize
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    gridXSize = Val(Left$(menuSelected, 2))
                    gridYSize = Val(Right$(menuSelected, 2))

                    If editMode = EDIT_SPRITE Then
                        spriteXGridSize = gridXSize
                        spriteYGridSize = gridYSize
                    Else
                        tileXGridSize = gridXSize
                        tileYGridsize = gridYSize
                    End If
                Case Else
            End Select
            'clipBoardhasData = 0
            Call initialDraw

        Case menu_Generate
            Select Case menuSelected
                Case "Generate .ODN"
                    Call GenerateFiles
            End Select
            Call initialDraw
        Case menu_Import
            Select Case menuSelected
                Case "Sprite as .png"
                    Call ImportSprite
            End Select
            Call initialDraw
    End Select
End Sub

Sub handleActiveKey ()
    Dim As Integer q
    If Len(activeKey) > 1 Then activeKey = "0" + Right$(activeKey, 1)

    Select Case activeKey
        Case Chr$(27)
            ' [Esc] key
            If activeMenu > menu_Base Then
                activeKey = ""
                activeMenu = menu_Base
                Call initialDraw
            Else
                q = abandonEdits
            End If
        Case "0I"
            '[PgUp] key
            boxPage = boxPage - 1
            If boxPage < 0 Then boxPage = 0
            Call drawPreviewBoxes
            Call paintPreviewBoxes
        Case "0Q"
            '[PgDn] key
            boxPage = boxPage + 1
            If boxPage > maxPage Then boxPage = maxPage
            Call drawPreviewBoxes
            Call paintPreviewBoxes
        Case "0K"
            ' shift left
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call shiftLeft
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
            End Select
        Case "0M"
            ' shift right
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call shiftRight
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
            End Select
        Case "0P"
            ' shift down
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call shiftDown
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
                Case EDIT_TILEMAP
                    tilemapPage = tilemapPage + 1
                    If tilemapPage > MAX_TILEMAP_PAGES - 1 Then
                        tilemapPage = MAX_TILEMAP_PAGES - 1
                    Else
                        Call drawTilemapGrid
                    End If
            End Select
        Case "0H"
            ' shift up
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call shiftUp
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
                Case EDIT_TILEMAP
                    tilemapPage = tilemapPage - 1
                    If tilemapPage < 0 Then
                        tilemapPage = 0
                    Else
                        Call drawTilemapGrid
                    End If
            End Select
        Case "H"
            ' flip horiz
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call flipHoriz
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
            End Select
        Case "V"
            ' flip vert
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call editInitialisation
                    Call flipVert
                    Call editFinish
                    Call drawCanvasGrid
                    Call updatePreviewBoxes(canvasIndex)
                    projectAmended = "Y"
            End Select

    End Select
End Sub

Function abandonEdits%
    Dim As Long q
    ' return 1 if its ok to continue
    ' i.e. no edits have been made
    ' or user has elected to abandon edits

    q = 2
    Select Case projectAmended
        Case "N"
            abandonEdits = 1
        Case "Y"
            q = _MessageBox("warning", "Abandon Edits Made?", "yesno", "question", q)

            If q = 0 Then
                If activeKey = Chr$(27) Then activeKey = ""
            End If
            abandonEdits = q
    End Select
End Function

Sub HandleButton_1_Click (x As Integer, y As Integer)
    Dim As Integer r

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            ' check and handle any menu clicks
            r = chkMenuClick(x, y)

            ' check and handle palette clicks
            If r = 0 Then
                r = chkPaletteClick(x, y)
            End If

            ' check and handle 512 colour clicks
            If r = 0 Then
                r = chk512ColourClick(x, y)
            End If

            ' check and handle canvas clicks
            If r = 0 Then
                r = chkCanvasClick(x, y)
            End If

            ' check and handle preview box click
            If r = 0 Then
                r = chkBoxClick(x, y)
            End If
        Case EDIT_TILEMAP
            ' check and handle any menu clicks
            r = chkMenuClick(x, y)

            ' check and handle Tilemap box click
            If r = 0 Then
                r = chkTilemapClick(x, y)
            End If

            ' check and handle preview box click
            If r = 0 Then
                r = chkBoxClick(x, y)
            End If

    End Select

End Sub

Sub HandleButton_2_Click (x As Integer, y As Integer)
    Dim As Integer r

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            ' check and handle palette clicks
            If r = 0 Then
                r = chkPaletteRClick(x, y)
            End If
    End Select

End Sub

Function chkMenuClick% (x As Integer, y As Integer)
    Dim As Integer iC, lX, lY, hX, hY
    Dim As String mT
    mT = ""
    iC = 1
    Do While menuData(iC, activeMenu, editMode, 1) <> 0 And mT = ""
        lX = menuData(iC, activeMenu, editMode, 1)
        lY = menuData(iC, activeMenu, editMode, 2)
        hX = menuData(iC, activeMenu, editMode, 3)
        hY = menuData(iC, activeMenu, editMode, 4)

        If x >= lX And x <= hX And y >= lY And y <= hY Then mT = menuText(activeMenu, iC, editMode)
        iC = iC + 1
    Loop

    If mT <> "" Then
        Call handleMenuClick(activeMenu, mT)
        chkMenuClick = 1
    Else
        chkMenuClick = 0
    End If
End Function

Function chkPaletteClick% (x, y)
    Dim As Integer lx, ly, hx, hy, bX, bY, box

    lx = palettelX
    ly = palettelY
    hx = palettehX
    hy = palettehY

    If x >= lx And x <= hx And y >= ly And y <= hy Then
        bX = (x - lx) \ CELL_SIZE_PIXELS
        bY = (y - ly) \ CELL_SIZE_PIXELS
        box = (bY * 64) + bX

        If box > 0 Then
            Select Case palettes(editMode, paletteSelected, box)
                Case NOT_SET
                    palettes(editMode, paletteSelected, box) = sel512Colour
                    canvasColour = box
                Case Else
                    sel512Colour = palettes(editMode, paletteSelected, box)
                    canvasColour = box
            End Select
            chkPaletteClick = 1
            Call drawPaletteGrid
            projectAmended = "Y"
        End If
    End If
End Function

Function chkPaletteRClick% (x, y)
    Dim As Integer lx, ly, hx, hy, bX, bY, box, q

    lx = palettelX
    ly = palettelY
    hx = palettehX
    hy = palettehY

    If x >= lx And x <= hx And y >= ly And y <= hy Then
        bX = (x - lx) \ CELL_SIZE_PIXELS
        bY = (y - ly) \ CELL_SIZE_PIXELS
        box = (bY * 64) + bX

        If palettes(editMode, paletteSelected, box) = NOT_SET Then
            palettes(editMode, paletteSelected, box) = sel512Colour
            canvasColour = box
            chkPaletteRClick = 1
            Call drawPaletteGrid
        Else
            q = _MessageBox("warning", "Overwrite Colour?", "yesno", "question", 0)
            If q = 1 Then
                palettes(editMode, paletteSelected, box) = sel512Colour
                If box <> 0 Then canvasColour = box
                chkPaletteRClick = 1
                Call drawPaletteGrid
                Call drawCanvasGrid
                Call drawPreviewBoxes
                Call paintPreviewBoxes
                projectAmended = "Y"

            End If
        End If
    End If
End Function

Function chk512ColourClick% (x, y)
    Dim As Integer lx, ly, hx, hy, bX, bY, box

    lx = colour512lX
    ly = colour512lY
    hx = colour512hX
    hy = colour512hY

    If x >= lx And x <= hx And y >= ly And y <= hy Then
        bX = (x - lx) \ CELL_SIZE_PIXELS
        bY = (y - ly) \ CELL_SIZE_PIXELS
        box = (bY * 64) + bX
        sel512Colour = box
        chk512ColourClick = 1
        Call draw512ColourGrid
    Else
        chk512ColourClick = 0
    End If
End Function

Function chkCanvasClick% (x, y)
    Dim As Integer lx, ly, hx, hy, bX, bY

    lx = canvaslX
    ly = canvaslY
    hx = canvashX
    hy = canvashY

    If x >= lx And x <= hx And y >= ly And y <= hy Then
        bX = (x - lx) \ CELL_SIZE_PIXELS
        bY = (y - ly) \ CELL_SIZE_PIXELS
        Call setSpriteColour(canvasIndex, bX, bY)
        chkCanvasClick = 1
        Call drawCanvasGrid
        Call updatePreviewBoxes(canvasIndex)
        projectAmended = "Y"
    Else
        chkCanvasClick = 0
    End If
End Function

Function chkTilemapClick% (x, y)
    Dim As Integer lx, ly, hx, hy, bX, bY, pix, z1, z2

    lx = tilemapGridlx
    ly = tilemapGridly
    hx = tilemapGridhx
    hy = tilemapGridhy
    pix = getWidth + 1

    If x >= lx And x <= hx And y >= ly And y <= hy Then
        bX = (x - lx) \ pix
        bY = (y - ly) \ pix
        chkTilemapClick = 1
        _PrintString (0, 400), Str$(bX) + Str$(bY) + "     "

        z1 = lx + (bX * pix) + 1
        z2 = ly + (bY * pix) + 1
        '        Line (z1, z2)-(z1 + 8, z2 + 8), ColorValue(511), BF
        If canvasIndex = tilePainted Then
            Put (z1, z2), bg1(), PSet
        Else
            paint1Tile (canvasIndex)
            Put (z1, z2), bg1(), PSet
        End If
        tilemapPages(tilemapPage, (bY * 40) + bX) = canvasIndex
        projectAmended = "Y"
    Else
        chkTilemapClick = 0
    End If
End Function


Function chkBoxClick% (x, y)
    Dim As Integer lx, ly, hx, hy, box

    chkBoxClick = 0
    For box = 0 To 31
        lx = boxdata(box, 1)
        ly = boxdata(box, 2)
        hx = boxdata(box, 3)
        hy = boxdata(box, 4)

        If x >= lx And x <= hx And y >= ly And y <= hy Then
            chkBoxClick = 1
            canvasIndex = (boxPage * boxesPerPage) + box
            Select Case editMode
                Case EDIT_SPRITE, EDIT_TILE
                    Call drawCanvasGrid
                    Call drawPreviewBoxes
                    Call paintPreviewBoxes
                Case EDIT_TILEMAP
                    Call drawPreviewBoxes
                    Call paintPreviewBoxes
            End Select
            Exit For
        End If
    Next box
End Function


Sub NewProject ()
    Dim As Integer l, l1, l2

    palettes(0, 0, 0) = 0
    palettes(0, 1, 0) = 0
    palettes(1, 0, 0) = 0
    palettes(1, 1, 0) = 0

    For l = 1 To 255
        palettes(0, 0, l) = NOT_SET
        palettes(0, 1, l) = NOT_SET
        palettes(1, 0, l) = NOT_SET
        palettes(1, 1, l) = NOT_SET
    Next l

    For l = 0 To 1
        For l1 = 0 To MAX_TILES - 1
            For l2 = 0 To 255
                sprites(l, l1, l2) = 0
            Next l2
        Next l1
    Next l
    editMode = EDIT_SPRITE
    maxPage = 1
    paletteSelected = 0
    spriteXGridSize = 16
    spriteYGridSize = 16
    tileXGridSize = 8
    tileYGridsize = 8
    gridXSize = 16
    gridYSize = 16
    clipBoardhasData = 0
End Sub

Sub LoadProject ()
    Dim As String indata, loadfileName, oDrive, ofileName, oExtension

    If abandonEdits = 0 Then Exit Sub

    loadfileName = _OpenFileDialog$("Open file", "", "*.stt", "", -1)
    If loadfileName = "" Then Exit Sub

    Call FileComponents(loadfileName, oDrive, ofileName, oExtension)
    fileName = ofileName
    fileDrive = oDrive

    Open loadfileName For Input As #1
    Do
        Line Input #1, indata
        If Not EOF(1) Then
            loadProjData (indata)
        End If
    Loop Until EOF(1)

    Close #1
    ChDir oDrive
    projectAmended = "N"
End Sub

Sub loadProjData (indata As String)
    Dim As Integer typeIndex, itemIndex
    Dim As Integer rows, cols, l1, l2, mybyte
    Dim As String itemType

    ' determine and set parameters for item being loaded
    itemType = Left$(indata, 1)

    ' indata will be of the form
    ' "'" - a comment that is ignored
    ' Sprite nnn
    ' Palette nnn
    ' Tile nnn
    ' Map nnn
    '
    ' the first letter indicates the type of asset being loaded
    ' the nnn is the index in its array
    If itemType = "'" Then Exit Sub
    If itemType = " " Then Exit Sub

    Select Case itemType
        Case "S"
            rows = 15
            cols = 15
        Case "P"
            rows = 15
            cols = 15
        Case "T"
            rows = 7
            cols = 7
        Case "M"
            rows = 31
            cols = 39
    End Select

    typeIndex = Val(Right$(indata, 3))
    itemIndex = 0

    ' load it in !
    For l1 = 0 To rows
        Line Input #1, indata
        For l2 = 0 To cols * 4 Step 4
            mybyte = Val(Mid$(indata, l2 + 1, 4))

            Select Case itemType
                Case "S"
                    sprites(0, typeIndex, itemIndex) = mybyte
                Case "P"
                    Select Case typeIndex
                        Case 0
                            palettes(0, 0, itemIndex) = mybyte
                        Case 1
                            palettes(0, 1, itemIndex) = mybyte
                        Case 2
                            palettes(1, 0, itemIndex) = mybyte
                        Case 3
                            palettes(1, 1, itemIndex) = mybyte
                    End Select
                Case "T"
                    sprites(1, typeIndex, itemIndex) = mybyte
                Case "M"
                    tilemapPages(typeIndex, itemIndex) = mybyte
            End Select

            itemIndex = itemIndex + 1
        Next l2
    Next l1
End Sub


Sub SaveProject ()
    Dim As Integer z1
    Dim As String savefileName, oDrive, ofileName, oExtension

    savefileName = _SaveFileDialog$("Save Project", fileName, "*.stt", "")
    If savefileName = "" Then Exit Sub

    Call FileComponents(savefileName, oDrive, ofileName, oExtension)
    fileName = ofileName
    fileDrive = oDrive

    Open fileDrive + fileName + ".stt" For Output As #1
    Print #1, "' " + fileName + ".stt"

    ' Process Sprite Data
    For z1 = 0 To MAX_SPRITES - 1
        Call writeProjData("S", z1)
    Next z1

    For z1 = 0 To MAX_TILES - 1
        Call writeProjData("T", z1)
    Next z1

    ' Process Sprite palette data
    Call writeProjData("P", 0)
    Call writeProjData("P", 1)
    Call writeProjData("P", 2)
    Call writeProjData("P", 3)

    ' write the "Page" data
    For z1 = 0 To MAX_TILEMAP_PAGES - 1
        Call writeProjData("M", z1)
    Next z1

    Close #1

    Call initialDraw
    projectAmended = "N"
End Sub

Sub writeProjData (itemType As String, typeIndex As Integer)
    Dim As Integer l1, items, itemsPerRow, itemcount
    Dim As String outdata, mybyte

    Select Case itemType
        Case "S"
            itemcount = 255
            itemsPerRow = 16
            outdata = "Sprite "
        Case "P"
            itemcount = 255
            itemsPerRow = 16
            outdata = "Palette "
        Case "T"
            itemcount = 63
            itemsPerRow = 8
            outdata = "Tile "
        Case "M"
            itemcount = 1279
            itemsPerRow = 40
            outdata = "Map "
    End Select

    outdata = outdata + lead0(typeIndex, 3)

    Print #1, "'"
    Print #1, outdata

    items = 0
    outdata = ""

    ' loop for all items in this data type
    For l1 = 0 To itemcount

        ' form data for this row
        Select Case itemType
            Case "S"
                mybyte = lead0(sprites(0, typeIndex, l1), 3)
            Case "T"
                mybyte = lead0(sprites(1, typeIndex, l1), 3)
            Case "P"
                Select Case typeIndex
                    Case 0
                        mybyte = lead0(palettes(0, 0, l1), 3)
                    Case 1
                        mybyte = lead0(palettes(0, 1, l1), 3)
                    Case 2
                        mybyte = lead0(palettes(1, 0, l1), 3)
                    Case 3
                        mybyte = lead0(palettes(1, 1, l1), 3)
                End Select
            Case "M"
                mybyte = lead0(tilemapPages(typeIndex, l1), 3)
        End Select

        outdata = outdata + mybyte + " "
        items = items + 1
        If items Mod itemsPerRow = 0 Then
            Print #1, outdata
            outdata = ""
        End If
    Next l1
End Sub


Sub FileComponents (iFullfileName As String, oDrivePath As String, ofileName As String, oExtension As String)
    Dim As Integer z, zz, ld, ls

    oDrivePath = ""
    ofileName = ""
    oExtension = ""

    z = Len(iFullfileName)
    If z = 0 Then Exit Sub

    ' find last dot
    For zz = z To 1 Step -1
        If Mid$(iFullfileName, zz, 1) = "." Then
            ld = zz
            Exit For
        End If
    Next zz

    ' find last slash
    For zz = z To 1 Step -1
        If Mid$(iFullfileName, zz, 1) = "/" Or Mid$(iFullfileName, zz, 1) = "\" Then
            ls = zz
            Exit For
        End If
    Next zz

    If ls > 0 Then oDrivePath = Left$(iFullfileName, ls)
    If ld > 0 Then oExtension = Right$(iFullfileName, z - ld)
    If ld > ls Then
        ofileName = Mid$(iFullfileName, ls + 1, z - ls - (z - ld + 1))
    Else
        ofileName = Mid$(iFullfileName, ls + 1, z - ls)

    End If
End Sub

Sub ImportSprite ()
    Dim As String loadfileName
    Dim img As Long
    Dim resizedimg As Long
    Dim As Integer x, y, c, p, t

    loadfileName = _OpenFileDialog$("Open file", "", "*.png", "", -1)
    If loadfileName = "" Then Exit Sub

    img = _LoadImage(loadfileName, 32)
    If img >= -1 Then
        _MessageBox "Import", "Failed to load ", "Info"
        Exit Sub
    End If

    ' create a new blank image with out sprite dimensions
    resizedimg = _NewImage(gridXSize, gridYSize, 32)
    If resizedimg >= -1 Then
        _MessageBox "Import", "Failed to create resized image", "Info"
        _FreeImage img
        Exit Sub
    End If

    ' resize original image to the new blank image
    _PutImage (0, 0)-(gridXSize - 1, gridYSize - 1), img, resizedimg

    ' free memory for original, we dont need it now
    _FreeImage img
    _Source resizedimg

    ' default the top leftg pixel as transparent
    ' any further finds of this colour will be OUR transparent
    t = pixelColour(0, 0)

    For y = 0 To gridYSize - 1
        For x = 0 To gridXSize - 1
            c = pixelColour(x, y)
            If c = t Then
                canvasColour = palettes(editMode, paletteSelected, 0)
                Call setSpriteColour(canvasIndex, x, y)
            Else
                p = SearchAddPalette(editMode, paletteSelected, c)
                canvasColour = p
                Call setSpriteColour(canvasIndex, x, y)
            End If
        Next x
    Next y
    Call drawCanvasGrid
    Call updatePreviewBoxes(canvasIndex)
    projectAmended = "Y"

    _FreeImage resizedimg
End Sub

Function pixelColour% (x As Integer, y As Integer)
    ' get the pixel colour, 8bit for each rgb channel
    ' reduce depth to 9 bit rgb
    ' return the 9bit value, 0-511
    Dim As Integer r, g, b, r9, g9, b9, c
    Dim pixel&
    pixel& = Point(x, y)
    r = _Red32(pixel&)
    g = _Green32(pixel&)
    b = _Blue32(pixel&)
    r9 = CInt((r / 255) * 7)
    g9 = CInt((g / 255) * 7)
    b9 = CInt((b / 255) * 7)
    c = Val("&B" + decToBin(r9, 3) + decToBin(g9, 3) + decToBin(b9, 3))

    pixelColour = c
End Function

Sub ImportPalette ()
    Print "Palette import not fully implemented yet."
    _Display

    Sleep 1
End Sub

Sub ExportSprite ()
    Print "Sprite export not fully implemented yet."
    _Display

    Sleep 1
End Sub

Sub ExportPalette ()
    Print "Palette export not fully implemented yet."
    _Display

    Sleep 1
End Sub

Sub writeFiles (s As String, b As Integer, toOdin As String, toTxt As String, toBin As String)
    Dim As _Unsigned _Byte mybyte

    If toOdin = "Y" Then Print #2, s

    If toBin = "Y" Then
        mybyte = b
        Put #3, byteswritten + 1, b
        byteswritten = byteswritten + 1
    End If

    If toTxt = "Y" Then Print #4, s

End Sub

Sub GenerateFiles ()
    Dim As String indata, func

    orgStart = 0
    byteswritten = 0

    Open fileDrive + "generate.def" For Input As #1
    If _FileExists(fileDrive + fileName + ".inc") Then Kill fileDrive + fileName + ".inc"
    If _FileExists(fileDrive + fileName + ".bin") Then Kill fileDrive + fileName + ".bin"
    If _FileExists(fileDrive + fileName + ".txt") Then Kill fileDrive + fileName + ".txt"

    Open fileDrive + fileName + ".inc" For Output As #2
    Open fileDrive + fileName + ".bin" For Binary As #3
    Open fileDrive + fileName + ".txt" For Output As #4
    Do
        Line Input #1, indata
        If Not EOF(1) Then
            If Left$(indata, 1) = "{" Then
                func = GetBracketedText(indata)
                Select Case func
                    Case "TILEPATTERNS"
                        Call GeneratePatterns(EDIT_TILE)
                    Case "TILEPALETTE"
                        Call generatePalette(EDIT_TILE, 0)
                    Case "SPRITEPATTERNS"
                        Call GeneratePatterns(EDIT_SPRITE)
                    Case "SPRITEPALETTE"
                        Call generatePalette(EDIT_SPRITE, 0)
                    Case "PAGEDATA"
                        Call generateMap(generate_rle)
                    Case "ORG"
                        Call generateOrg(indata)
                    Case "ZXSPECTRUMNEXT"
                        Call generateDevice
                End Select
            Else
                If Left$(indata, 1) = "'" Then
                Else
                    Call writeFiles(indata, 0, "Y", "N", "N")
                End If
            End If
        End If
    Loop Until EOF(1)
    Close #1
    Close #2
    Close #3
    Close #4
    _MessageBox ".ODN Generate", "Ensure .Bin file only loads bytes : " + Str$(byteswritten), "info"
End Sub

Sub generateDevice
    Call writeFiles("device ZXSPECTRUMNEXT", 0, "Y", "N", "N")
    Call writeFiles("", 0, "Y", "N", "N")
End Sub

Sub generateOrg (indata As String)
    Dim As String orgVal

    orgVal = Right$(indata, 4)
    orgStart = Val("&H" + orgVal)
    Call writeFiles("ORG $" + orgVal, 0, "Y", "N", "N")
End Sub

Sub GeneratePatterns (patternType As Integer)
    Dim As Integer pixelCount, CELL_SIZE_PIXELSPerRow, CELL_SIZE_PIXELSPerByte
    Dim As Integer c, l, x, z
    Dim As String myByte, outData, pixel1, pixel2, itemType

    If patternType = EDIT_SPRITE Then
        pixelCount = 255
        CELL_SIZE_PIXELSPerRow = 16
        CELL_SIZE_PIXELSPerByte = 1
        itemType = "Sprite"
    Else
        pixelCount = 63
        CELL_SIZE_PIXELSPerRow = 8
        CELL_SIZE_PIXELSPerByte = 2
        itemType = "Tile"
    End If
    Call writeFiles(itemType + "Patterns:", 0, "Y", "N", "N")

    For l = 0 To getItemGenCount(patternType)
        z = 0
        Call writeFiles("; " + itemType + lead0(l, 3), 0, "Y", "Y", "N")
        'Call writeFiles("defs " + lead0((pixelCount + 1) / CELL_SIZE_PIXELSPerByte, 3) + ", 0", 0, "Y", "N", "N")
        outData = "DB "

        For x = 0 To pixelCount Step CELL_SIZE_PIXELSPerByte

            If patternType = EDIT_SPRITE Then
                ' each pixel is 8 bits, so 1 CELL_SIZE_PIXELS per byte
                c = sprites(0, l, z)
                pixel1 = decToBin(c, 8)

                myByte = decToHex(Val("&B" + pixel1))
                Call writeFiles("", c, "N", "N", "Y")
                z = z + 1
            Else
                ' each pixel is 4 bits, so 2 CELL_SIZE_PIXELS per byte
                c = sprites(1, l, z)
                pixel1 = decToBin(c, 4)

                c = sprites(1, l, z + 1)
                pixel2 = decToBin(c, 4)

                myByte = decToHex(Val("&B" + pixel1 + pixel2))
                Call writeFiles("", Val("&B" + pixel1 + pixel2), "N", "N", "Y")
                z = z + 2
            End If

            ' append each byte value
            outData = outData + myByte + ","

            '         If z Mod CELL_SIZE_PIXELSPerRow = 0 Then
            'If z Mod (pixelCount + 1) = 0 Then
            If z Mod 32 = 0 Then
                Call writeFiles(Left$(outData, Len(outData) - 1), 0, "Y", "Y", "N")
                outData = "DB "
            End If
        Next x
    Next l

    If patternType = EDIT_SPRITE Then
        Call writeFiles("endofSprites:", 0, "Y", "N", "N")
    End If

End Sub

Sub generatePalette (patternType As Integer, palNo As Integer)
    Dim As Integer l, c, z
    Dim As String mybyte, itemtype, outdata

    If patternType = EDIT_SPRITE Then
        itemtype = "SpritePalette" + LTrim$(Str$(palNo))
    Else
        itemtype = "TilePalette" + LTrim$(Str$(palNo))
    End If
    Call writeFiles("", 0, "Y", "Y", "N")
    Call writeFiles(itemtype + ":", 0, "Y", "Y", "N")

    z = 0
    outdata = ""
    For l = 0 To getPaletteItemCount(patternType, palNo)
        c = palettes(patternType, palNo, l)
        If c = NOT_SET Then c = 0
        mybyte = decToHex9bit(c)
        outdata = outdata + mybyte + ","
        z = z + 1
        If z Mod 8 = 0 Then
            Call writeFiles("DB " + Left$(outdata, Len(outdata) - 1), 0, "Y", "Y", "N")
            outdata = ""
        End If
    Next l

    If Len(outdata) > 0 Then
        Call writeFiles("DB " + Left$(outdata, Len(outdata) - 1), 0, "Y", "Y", "N")
    End If

    Call writeFiles(itemtype + "Len EQU ($ - " + itemtype + ") / 2", 0, "Y", "N", "N")
    Call writeFiles("", 0, "Y", "Y", "N")
End Sub

Sub generateMap (gentype As Integer)
    Dim As Integer l, x, z, t, t1, c, gt
    Dim As String outdata, page

    Call writeFiles("; Page Data, aka the tilemap pages", 0, "Y", "N", "N")

    For l = 0 To getItemGenCount(EDIT_TILEMAP)
        Call writeFiles("", 0, "Y", "N", "N")

        Select Case gentype
            Case generate_raw
                GoSub generateRaw
            Case generate_rle
                GoSub generateRLE
        End Select
    Next l

    Exit Sub

    generateRaw:
    Call writeFiles("page" + lead0(l, 3) + ":", 0, "Y", "Y", "N")
    'Call writeFiles("defs 1280, 0", 0, "Y", "N", "N")

    z = 0
    outdata = "DB "
    For x = 0 To 1279
        outdata = outdata + decToHex(tilemapPages(l, x)) + ","
        Call writeFiles("", tilemapPages(l, x), "N", "N", "Y")
        z = z + 1
        If z Mod 40 = 0 Then
            Call writeFiles(Left$(outdata, Len(outdata) - 1), 0, "N", "Y", "N")
            outdata = "DB "
        End If
    Next x
    Return

    generateRLE:
    page = "page" + lead0(l, 3)
    Call writeFiles(page + ":", 0, "Y", "Y", "N")

    gt = 0
    t = tilemapPages(l, 0)
    c = 0
    z = 0
    For x = 0 To 1279
        t1 = tilemapPages(l, x)
        If t1 = t Then
            c = c + 1
            If c = 255 Then
                Call writeFiles("", t, "N", "N", "Y")
                Call writeFiles("", c, "N", "N", "Y")
                Call writeFiles("DB " + lead0(t, 3) + "," + lead0(c, 3), 0, "Y", "Y", "N")
                gt = gt + c
                z = z + 2
                c = 0
            End If
        Else
            Call writeFiles("", t, "N", "N", "Y")
            Call writeFiles("", c, "N", "N", "Y")
            Call writeFiles("DB " + lead0(t, 3) + "," + lead0(c, 3), 0, "Y", "Y", "N")
            gt = gt + c
            z = z + 2
            c = 1
            t = t1
        End If
    Next x
    If t1 = t And c > 1 Then
        Call writeFiles("", t, "N", "N", "Y")
        Call writeFiles("", c, "N", "N", "Y")
        Call writeFiles("DB " + lead0(t, 3) + "," + lead0(c, 3), 0, "Y", "Y", "N")
        gt = gt + c
        z = z + 2
    End If

    If gt <> 1280 Then
        _MessageBox "Generate", "warning " + page + " bytes written " + Str$(gt), "info"
    End If
    'Call writeFiles("end" + page + ":", 0, "Y", "Y", "N")
    'Call writeFiles("          assert end" + page + " - " + page + " = 1280", 0, "Y", "Y", "N")
    'Call writeFiles("page" + lead0(l, 3) + ":", 0, "Y", "N", "N")
    'Call writeFiles("defs" + Str$(z) + ", 0", 0, "Y", "N", "N")
    Return
End Sub

Function getItemGenCount% (patterntype As Integer)
    Dim As Integer l, l1, z, x, x1
    Select Case patterntype
        Case EDIT_SPRITE
            l = MAX_SPRITES - 1
            x = 255
        Case EDIT_TILE
            l = MAX_TILES - 1
            x = 63
        Case EDIT_TILEMAP
            l = MAX_TILEMAP_PAGES - 1
            x = 1279
    End Select

    z = 0
    For l1 = 0 To l
        For x1 = 0 To x
            Select Case patterntype
                Case EDIT_SPRITE
                    If sprites(0, l1, x1) > 0 Then z = l1
                Case EDIT_TILE
                    If sprites(1, l1, x1) > 0 Then z = l1
                Case EDIT_TILEMAP
                    If tilemapPages(l1, x1) > 0 Then z = l1
            End Select
        Next x1
    Next l1
    getItemGenCount = z
End Function

Function getPaletteItemCount (p As Integer, p1 As Integer)
    Dim As Integer l, z

    z = 255
    For l = 255 To 0 Step -1
        If palettes(p, p1, l) <> NOT_SET Then
            z = l
            Exit For
        End If
    Next l

    getPaletteItemCount = z
End Function

Function getSpriteValue% (s As Integer, x As Integer, y As Integer)
    Dim As Integer s1, x2, c

    ' get the sprite/tile colour index
    ' i.e. the offset into the sprite/tile palette

    s1 = getSpriteIndex(s, x, y)
    x2 = getOffset(x, y)
    c = sprites(editAssetType, s1, x2)

    getSpriteValue = c
End Function

Function getSpriteColour~& (s As Integer, x As Integer, y As Integer)
    Dim As Integer c
    Dim As Long c1

    ' return the sprite/tile RGB colour value

    c = getSpriteValue(s, x, y)
    c1 = ColorValue(palettes(editAssetType, paletteSelected, c))
    getSpriteColour = c1
End Function

Sub setSpriteColour (s As Integer, x As Integer, y As Integer)
    Dim As Integer s1, offset

    s1 = getSpriteIndex(s, x, y)
    offset = getOffset(x, y)

    If palettes(editMode, paletteSelected, canvasColour) <> NOT_SET Then
        If sprites(editMode, s1, offset) = 0 Then
            sprites(editMode, s1, offset) = canvasColour
        Else
            sprites(editMode, s1, offset) = 0
        End If
    End If
End Sub

Function getSpriteIndex% (s As Integer, x As Integer, y As Integer)
    Dim As Integer s1, s2, s3, z

    ' given the base sprite/tile index s and x/y pixel coordinates
    ' then return the sprite/tile index taking into account the edit size may be greater than a standard sprite or tile

    z = getWidth
    s2 = x \ z
    s3 = ((y \ z) * (gridXSize \ z))
    s1 = s + s2 + s3
    getSpriteIndex = s1
End Function

Function getOffset% (x As Integer, y As Integer)
    Dim As Integer z, x1, y1

    ' an individual sprite or tile is 16x16 or 8x8 CELL_SIZE_PIXELS but we store as an array 0-255 of integers
    ' here we return the offset into the array given the X & Y coordinates
    ' NB the offset takes into account the edit size may be greater than a standard sprite or tile

    z = getWidth

    x1 = x Mod z
    y1 = (y Mod z) * z
    getOffset = x1 + y1

End Function

Function getWidth%
    ' simply return the pixel width of the edit type

    If editMode = EDIT_SPRITE Then
        getWidth = 16
    Else
        getWidth = 8
    End If
End Function

Sub setupColorTable
    Dim As Integer z, rrr, ggg, bbb
    Dim As String rgb9, t1, t2, t3

    For z = 0 To 511
        rgb9 = decToBin(z, 9)
        t1 = Mid$(rgb9, 1, 3)
        t2 = Mid$(rgb9, 4, 3)
        t3 = Mid$(rgb9, 7, 3)
        rrr = RGBconv%(t1)
        ggg = RGBconv%(t2)
        bbb = RGBconv%(t3)

        ColorTable(z, 1) = rrr
        ColorTable(z, 2) = ggg
        ColorTable(z, 3) = bbb
        ColorValue(z) = _RGB(rrr, ggg, bbb)
    Next z

End Sub

Sub clipBoardCopy (editMode As Integer)
    Dim As Integer x, y, canvasindex1
    Dim As String otext

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            For x = 0 To 63
                For y = 0 To 63
                    clipBoard(x, y) = 0
                Next y
            Next x

            For x = 0 To gridXSize - 1
                For y = 0 To gridYSize - 1
                    clipBoard(x, y) = getSpriteValue(canvasIndex, x, y)
                Next y
            Next x
            clipBoardhasData = 1
            clipBoardSource = editMode
            canvasindex1 = getSpriteIndex(canvasIndex, gridXSize - 1, gridYSize - 1)
            otext = lead0(canvasIndex, 3)
            If canvasindex1 <> canvasIndex Then otext = otext + " to " + lead0(canvasindex1, 3)
            If editMode = EDIT_SPRITE Then
                otext = "Sprite " + otext + " copied to clipboard"
            Else
                otext = "Tile " + otext + " copied to clipboard"
            End If
            _MessageBox "ClipBoard", otext, "info"

        Case EDIT_TILEMAP
            For x = 0 To 1279
                tilemapClipboard(x) = tilemapPages(tilemapPage, x)
            Next x
            clipBoardhasData = 1
            clipBoardSource = editMode
            otext = "Tilemap page " + lead0(tilemapPage, 2) + " copied to clipboard"
            _MessageBox "ClipBoard", otext, "info"

    End Select
End Sub

Sub clipBoardCut (editMode As Integer)
    Dim As Integer x, y

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            Call clipBoardCopy(editMode)
            For x = 0 To gridXSize - 1
                For y = 0 To gridYSize - 1
                    sprites(editMode, getSpriteIndex(canvasIndex, x, y), getOffset(x, y)) = 0
                Next y
            Next x
            projectAmended = "Y"

        Case EDIT_TILEMAP
            Call clipBoardCopy(editMode)
            For x = 0 To 1279
                tilemapPages(tilemapPage, x) = 0
            Next x
            projectAmended = "Y"
    End Select
End Sub

Sub clipBoardPaste (editMode As Integer)
    Dim As Integer x, y

    ' if the source was not the same as destination
    ' then try to get a decent palette colour match
    If editMode <> clipBoardSource Then
        For x = 0 To gridXSize - 1
            For y = 0 To gridYSize - 1
                clipBoard(x, y) = blah(x, y)
            Next y
        Next x
    End If

    Select Case editMode
        Case EDIT_SPRITE, EDIT_TILE
            If clipBoardhasData = 1 Then
                For x = 0 To gridXSize - 1
                    For y = 0 To gridYSize - 1
                        sprites(editMode, getSpriteIndex(canvasIndex, x, y), getOffset(x, y)) = clipBoard(x, y)
                    Next y
                Next x
            End If
            projectAmended = "Y"

        Case EDIT_TILEMAP
            If clipBoardhasData = 1 Then
                For x = 0 To 1279
                    tilemapPages(tilemapPage, x) = tilemapClipboard(x)
                Next x
            End If
            projectAmended = "Y"

    End Select
End Sub

Function blah% (x As Integer, y As Integer)
    Dim As Integer z, c2, c1, c, diff, diff1, nearest

    c = clipBoard(x, y)
    c2 = palettes(clipBoardSource, paletteSelected, c)
    diff1 = 512
    For z = 0 To 255
        c1 = palettes(editMode, paletteSelected, z)
        diff = Abs(c1 - c2)
        If diff < diff1 Then
            diff1 = diff
            nearest = z
        End If
    Next z
    blah = nearest
End Function

Sub editInitialisation
    Dim As Integer x, y
    For x = 0 To 63
        For y = 0 To 63
            tempSprite(x, y) = 0
        Next y
    Next x

    For x = 0 To gridXSize - 1
        For y = 0 To gridYSize - 1
            tempSprite(x, y) = getSpriteValue(canvasIndex, x, y)
        Next y
    Next x
End Sub

Sub shiftRight
    Dim As Integer x, y
    ' shift right
    For y = 0 To gridYSize - 1
        For x = gridXSize To 1 Step -1
            tempSprite(x, y) = tempSprite(x - 1, y)
        Next x
        tempSprite(0, y) = tempSprite(gridXSize, y)
    Next y
End Sub

Sub shiftLeft
    Dim As Integer x, y, t
    ' shift left
    For y = 0 To gridYSize - 1
        t = tempSprite(0, y)
        For x = 0 To gridXSize
            tempSprite(x, y) = tempSprite(x + 1, y)
        Next x
        tempSprite(gridXSize - 1, y) = t
    Next y
End Sub

Sub shiftDown
    Dim As Integer x, y
    ' shift down
    For x = 0 To gridXSize - 1
        For y = gridYSize To 1 Step -1
            tempSprite(x, y) = tempSprite(x, y - 1)
        Next y
        tempSprite(x, 0) = tempSprite(x, gridYSize)
    Next x
End Sub

Sub shiftUp
    Dim As Integer x, y, t
    ' shift up
    For x = 0 To gridXSize - 1
        t = tempSprite(x, 0)
        For y = 0 To gridYSize
            tempSprite(x, y) = tempSprite(x, y + 1)
        Next y
        tempSprite(x, gridYSize - 1) = t
    Next x
End Sub

Sub flipHoriz
    Dim As Integer y, z1, z2, v1, v2

    For y = 0 To gridYSize - 1
        z1 = gridXSize - 1
        z2 = 0
        Do
            v1 = tempSprite(z2, y)
            v2 = tempSprite(z1, y)
            tempSprite(z2, y) = v2
            tempSprite(z1, y) = v1
            z1 = z1 - 1
            z2 = z2 + 1
        Loop Until z1 < gridXSize \ 2
    Next y
End Sub

Sub flipVert
    Dim As Integer x, z1, z2, v1, v2

    For x = 0 To gridXSize - 1
        z1 = gridYSize - 1
        z2 = 0
        Do
            v1 = tempSprite(x, z2)
            v2 = tempSprite(x, z1)
            tempSprite(x, z2) = v2
            tempSprite(x, z1) = v1
            z1 = z1 - 1
            z2 = z2 + 1
        Loop Until z1 < gridYSize \ 2
    Next x
End Sub

Sub editFinish
    Dim As Integer c, x, y
    ' put back into sprite
    For x = 0 To gridXSize - 1
        For y = 0 To gridYSize - 1
            c = tempSprite(x, y)
            Call setSpriteColour1(x, y, c)
        Next y
    Next x
End Sub


Sub setSpriteColour1 (x As Integer, y As Integer, myColour As Integer)
    Dim As Integer s1, offset

    s1 = getSpriteIndex(canvasIndex, x, y)
    offset = getOffset(x, y)

    sprites(editMode, s1, offset) = myColour

End Sub


Function lead0$ (curval As Integer, nodigits As Integer)
    lead0$ = Right$("0000" + LTrim$(Str$(curval)), nodigits)
End Function

Function decToBin$ (c As Integer, n As Integer)
    Dim As String z, z1

    z = String$(n, "0")
    z1 = _Bin$(c)
    decToBin = Right$(z + z1, n)
End Function

Function decToHex$ (c As Integer)
    Dim As String z, z1

    z = String$(4, "0")
    z1 = Hex$(c)
    decToHex = "$" + Right$(z + z1, 2)
End Function

Function decToHex9bit$ (c As Integer)
    Dim As String rgb9, res1, res2
    Dim As Integer cc

    rgb9 = decToBin(c, 9)
    cc = Val("&B" + Left$(rgb9, 8))

    res1 = decToHex(cc)
    res2 = ",$00"
    If Right$(rgb9, 1) = "1" Then res2 = ",$01"

    decToHex9bit = res1 + res2
End Function

Function RGBconv% (x As String)
    Select Case x
        Case "000"
            RGBconv% = 0
        Case "001"
            RGBconv% = 36
        Case "010"
            RGBconv% = 73
        Case "011"
            RGBconv% = 109
        Case "100"
            RGBconv% = 146
        Case "101"
            RGBconv% = 182
        Case "110"
            RGBconv% = 219
        Case "111"
            RGBconv% = 255
    End Select
End Function

Sub loadMenuData ()
    Dim As Integer iC, am, lX, lY, hX, hY, eT
    Dim As Integer ix, iy, mwidth, mheight, mspacer

    Dim As String mT
    ix = 10
    iy = 12
    mwidth = 80
    mheight = 20
    mspacer = 10

    Restore
    Do While am <> -1
        Read am, iC, eT, mT
        'Print am, iC, eT, mT
        If am <> -1 Then
            If am = 0 Then
                lX = ix + ((iC - 1) * (mwidth + mspacer))
                lY = iy
                hX = lX + mwidth
                hY = lY + mheight
            Else
                lX = ix + ((am - 1) * (mwidth + mspacer))
                lY = iy + ((iC) * (mheight + mspacer))
                hX = lX + mwidth
                hY = lY + mheight
            End If

            menuData(iC, am, eT, 1) = lX
            menuData(iC, am, eT, 2) = lY
            menuData(iC, am, eT, 3) = hX
            menuData(iC, am, eT, 4) = hY
            menuText(am, iC, eT) = mT
        End If

    Loop
End Sub

Function GetBracketedText$ (inputString$)
    Dim startPos As Integer
    Dim endPos As Integer

    startPos = InStr(inputString$, "{")
    endPos = InStr(inputString$, "}")

    If startPos > 0 And endPos > startPos Then
        GetBracketedText$ = Mid$(inputString$, startPos + 1, endPos - startPos - 1)
    Else
        GetBracketedText$ = ""
    End If
End Function

Function SearchAddPalette% (editMode As Integer, paletteSelected As Integer, c As Integer)
    Dim As Integer l, f, mp

    ' search the palette to see if colour c is present, return palette index if it is found
    f = -1
    For l = 1 To 255
        mp = palettes(editMode, paletteSelected, l)
        If mp = c Then
            f = l
            Exit For
        End If
    Next l

    ' if colour C is not found, then add it into the palette and return the palette index used
    If f = -1 Then
        For l = 1 To 255
            mp = palettes(editMode, paletteSelected, l)
            If mp = NOT_SET Then
                f = l
                palettes(editMode, paletteSelected, l) = c
                Exit For
            End If
        Next l
    End If
    SearchAddPalette = f
End Function

'    1st digit is the active menu
'    2nd digit is the menu item
'    3rd digit is the edit type 0=sprite, 1=tile, 2=tilemap

Data 0,1,0,"File ..."
Data 1,1,0,"New ..."
Data 1,2,0,"Load ..."
Data 1,3,0,"Save ..."

Data 0,1,1,"File ..."
Data 1,1,1,"New ..."
Data 1,2,1,"Load ..."
Data 1,3,1,"Save ..."

Data 0,1,2,"File ..."
Data 1,1,2,"New ..."
Data 1,2,2,"Load ..."
Data 1,3,2,"Save ..."

Data 0,2,0,"Edit"
Data 2,1,0,"Copy"
Data 2,2,0,"Cut"
Data 2,3,0,"Paste"

Data 0,2,1,"Edit"
Data 2,1,1,"Copy"
Data 2,2,1,"Cut"
Data 2,3,1,"Paste"

Data 0,2,2,"Edit"
Data 2,1,2,"Copy"
Data 2,2,2,"Cut"
Data 2,3,2,"Paste"

Data 0,3,0,"Edit Type"
Data 3,1,0,"Sprite"
Data 3,2,0,"Tile"
Data 3,3,0,"TileMap"

Data 0,3,1,"Edit Type"
Data 3,1,1,"Sprite"
Data 3,2,1,"Tile"
Data 3,3,1,"TileMap"

Data 0,3,2,"Edit Type"
Data 3,1,2,"Sprite"
Data 3,2,2,"Tile"
Data 3,3,2,"TileMap"

Data 0,4,0,"Grid Size"
Data 4,1,0,"16 x 16"
Data 4,2,0,"16 x 32"
Data 4,3,0,"32 x 16"
Data 4,4,0,"32 x 32"

Data 0,4,1,"Grid Size"
Data 4,1,1,"8 x 8"
Data 4,2,1,"8 x 16"
Data 4,3,1,"16 x 8"
Data 4,4,1,"16 x 16"
Data 4,5,1,"32 x 32"

Data 0,4,2,"Grid Size"
Data 4,1,2,"40 x 32"


Data 0,5,0,"Generate"
Data 5,1,0,"Generate .ODN"

Data 0,5,1,"Generate"
Data 5,1,1,"Generate .ODN"

Data 0,5,2,"Generate"
Data 5,1,2,"Generate .ODN"

Data 0,6,0,"Import"
Data 6,1,0,"Sprite as .png"
Data -1,-1,-1,""



