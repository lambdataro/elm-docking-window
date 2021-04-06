module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Dec exposing (Decoder)
import List.Extra
import Material.Icons.Action as ActionIcons
import Material.Icons.Content as ContentIcons
import Material.Icons.Navigation as NavigationIcons
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Flags =
    { winWidth : Float
    , winHeight : Float
    }


type alias Model =
    { winSize : Size
    , mousePos : Pos
    , draggingStatus : DraggingStatus
    , toolbar : Toolbar
    , nextPaneId : PaneId
    , paneDict : PaneDict
    , floatings : List PaneId
    , dockingAreaDict : DockingAreaDict
    , dockingRoot : DockingAreaId
    , nextDockingAreaId : DockingAreaId
    , splitterHoverStatus : HoverStatus SplitterId
    , markerHoverStatus : HoverStatus MarkerId
    , nextBackgroundHue : Int
    }


type alias PaneId =
    Int


type alias DockingAreaId =
    Int


type DraggingStatus
    = NotDragging
    | DraggingSplitter SplitterId
    | DraggingPane Pane Offset
    | ResizingPane Pane


type alias Toolbar =
    { hoverStatus : HoverStatus ToolId
    , height : Float
    }


type alias SplitterId =
    { areaId : DockingAreaId
    , index : Int
    }


type HoverStatus a
    = NotHovered
    | Hovered a


type alias PaneDict =
    Dict PaneId Pane


type alias Pane =
    { id : PaneId
    , pos : Pos
    , size : Size
    , title : String
    , status : PaneStatus
    }


type PaneStatus
    = Docked DockingAreaId
    | Floating


type alias DockingAreaDict =
    Dict DockingAreaId DockingArea


type alias DockingArea =
    { id : DockingAreaId
    , pos : Pos
    , size : Size
    , orientation : Orientation
    , items : List DockingItem
    }


type Orientation
    = Horizontal
    | Vertical


type alias DockingItem =
    { itemType : DockingItemType
    , factor : Float
    }


type DockingItemType
    = PaneItem PaneId
    | DockingAreaItem DockingAreaId


init : Flags -> ( Model, Cmd Msg )
init { winWidth, winHeight } =
    let
        dockingAreaDict : DockingAreaDict
        dockingAreaDict =
            Dict.singleton 0
                { id = 0
                , pos = { x = 0, y = 0 }
                , size = { width = 50, height = 50 }
                , orientation = Horizontal
                , items = []
                }
    in
    withCmdNone
        { winSize = { width = winWidth, height = winHeight }
        , mousePos = { x = 0, y = 0 }
        , draggingStatus = NotDragging
        , toolbar = { hoverStatus = NotHovered, height = 36 }
        , nextPaneId = 1
        , paneDict = Dict.empty
        , floatings = []
        , dockingAreaDict = dockingAreaDict
        , dockingRoot = 0
        , nextDockingAreaId = 1
        , splitterHoverStatus = NotHovered
        , markerHoverStatus = NotHovered
        , nextBackgroundHue = 0
        }



-- UPDATE


type Msg
    = BrowserResize Size
    | MouseMove Pos
    | MouseUp Pos
    | ToolbarHoverEvent ToolbarHoverEvent
    | AddNewPane
    | SplitterHoverEvent SplitterHoverEvent
    | StartSplitterDragging SplitterId
    | PaneTitleMouseDown PaneId
    | PaneBodyMouseDown PaneId
    | MarkerMouseEvent MarkerMouseEvent
    | DockPane MarkerId PaneId
    | ResizeHandleMouseDown PaneId
    | CloseButtonMouseDown PaneId


type MouseHoverEvent a
    = OnMouseOver a
    | OnMouseOut a


type alias ToolbarHoverEvent =
    MouseHoverEvent ToolId


type alias SplitterHoverEvent =
    MouseHoverEvent SplitterId


type alias MarkerMouseEvent =
    MouseHoverEvent MarkerId


type MarkerId
    = RootCenterMarker
    | RootArrowMarker ArrowMarker
    | PaneArrowMarker ArrowMarker PaneId


type ArrowMarker
    = LeftMarker
    | RightMarker
    | UpMarker
    | DownMarker


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        resizeMsg : (Size -> a) -> Int -> Int -> a
        resizeMsg f width height =
            f { width = toFloat width, height = toFloat height }

        mousePosDecoder : (Pos -> a) -> Decoder a
        mousePosDecoder f =
            Dec.map2 (\x y -> f { x = x, y = y })
                (Dec.field "offsetX" Dec.float)
                (Dec.field "offsetY" Dec.float)
    in
    Sub.batch
        [ Browser.Events.onResize (resizeMsg BrowserResize)
        , Browser.Events.onMouseMove (mousePosDecoder MouseMove)
        , Browser.Events.onMouseUp (mousePosDecoder MouseUp)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- マウス移動に反応する
        updateMouseMoveEvent : Model -> ( Model, Cmd Msg )
        updateMouseMoveEvent model0 =
            case model0.draggingStatus of
                NotDragging ->
                    withCmdNone model0

                DraggingSplitter splitterId ->
                    withCmdNone <| updateRootDockingLayout <| updateDraggingSplitterEvent splitterId model0

                DraggingPane pane mouseOffset ->
                    case pane.status of
                        Docked dockingAreaId ->
                            withCmdNone <| updateRootDockingLayout <| updateDraggingDockedPaneEvent pane mouseOffset dockingAreaId model0

                        Floating ->
                            withCmdNone <| updateRootDockingLayout <| updateDraggingFloatingPaneEvent pane mouseOffset model0

                ResizingPane pane ->
                    case pane.status of
                        Docked _ ->
                            withCmdNone model0

                        Floating ->
                            withCmdNone <| updateResizingFloatingPaneEvent pane model0

        updateDraggingFloatingPaneEvent : Pane -> Offset -> Model -> Model
        updateDraggingFloatingPaneEvent pane offset model0 =
            { model0 | paneDict = Dict.insert pane.id { pane | pos = addPos model0.mousePos (negOffset offset) } model0.paneDict }

        updateDraggingDockedPaneEvent : Pane -> Offset -> DockingAreaId -> Model -> Model
        updateDraggingDockedPaneEvent pane offset areaId model0 =
            if distance (addPos pane.pos offset) model0.mousePos < 50 then
                model0

            else
                separatePane areaId pane offset model0

        updateResizingFloatingPaneEvent : Pane -> Model -> Model
        updateResizingFloatingPaneEvent pane model0 =
            let
                updatedPane =
                    { pane | size = offsetSize (subPos model0.mousePos pane.pos) }
            in
            { model0 | paneDict = Dict.insert pane.id updatedPane model0.paneDict }

        separatePane : DockingAreaId -> Pane -> Offset -> Model -> Model
        separatePane areaId pane offset model0 =
            let
                paneWidth =
                    300

                paneHeight =
                    200

                updatedPane =
                    { pane
                        | status = Floating
                        , pos = addPos model0.mousePos (negOffset offset)
                        , size = { width = paneWidth, height = paneHeight }
                    }

                modifiedOffset =
                    if offset.offsetX > paneWidth then
                        { offset | offsetX = paneWidth / 3 }

                    else
                        offset
            in
            { model0
                | paneDict = Dict.insert pane.id updatedPane model0.paneDict
                , dockingAreaDict =
                    Dict.update areaId (Maybe.map (removeDockingAreaPane (PaneItem pane.id))) model0.dockingAreaDict
                , floatings = pane.id :: model0.floatings
                , draggingStatus = DraggingPane updatedPane modifiedOffset
            }

        updateDraggingSplitterEvent : SplitterId -> Model -> Model
        updateDraggingSplitterEvent splitterId model0 =
            { model0
                | dockingAreaDict =
                    Dict.update splitterId.areaId
                        (Maybe.map (\area -> moveSplitterPosition area splitterId.index))
                        model0.dockingAreaDict
            }

        moveSplitterPosition : DockingArea -> Int -> DockingArea
        moveSplitterPosition area index =
            let
                getAt : Int -> Maybe DockingItem
                getAt idx =
                    List.Extra.getAt idx area.items

                setAt : Int -> DockingItem -> List DockingItem -> List DockingItem
                setAt =
                    List.Extra.setAt

                mouseOffset : Offset
                mouseOffset =
                    { offsetX = 0, offsetY = model.toolbar.height }
            in
            case ( area.orientation, getAt index, getAt (index + 1) ) of
                ( Horizontal, Just leftItem, Just rightItem ) ->
                    newHorizontalSplitterPosition mouseOffset area index ( leftItem, rightItem )
                        |> (\( newLeftItem, newRightItem ) ->
                                { area | items = setAt index newLeftItem (setAt (index + 1) newRightItem area.items) }
                           )

                ( Vertical, Just upperItem, Just lowerItem ) ->
                    newVerticalSplitterPosition mouseOffset area index ( upperItem, lowerItem )
                        |> (\( newUpperItem, newLowerItem ) ->
                                { area | items = setAt index newUpperItem (setAt (index + 1) newLowerItem area.items) }
                           )

                _ ->
                    area

        newHorizontalSplitterPosition : Offset -> DockingArea -> Int -> ( DockingItem, DockingItem ) -> ( DockingItem, DockingItem )
        newHorizontalSplitterPosition mouseOffset area index ( left, right ) =
            let
                leftX =
                    area.pos.x + area.size.width * (List.take index area.items |> List.foldl (\item -> (+) item.factor) 0)

                -- splitter の左にあるペインの幅
                leftWidth =
                    area.size.width * left.factor

                -- splitter の右にあるペインの幅
                rightWidth =
                    area.size.width * right.factor

                -- マウスで移動できる範囲のマージン
                margin =
                    20

                -- 移動できる範囲に制限したX座標
                trimmedX =
                    max (leftX + margin) (min (leftX + leftWidth + rightWidth - margin) (model.mousePos.x - mouseOffset.offsetX))

                newLeftFactor =
                    (trimmedX - leftX) / (leftWidth + rightWidth) * (left.factor + right.factor)

                newRightFactor =
                    (left.factor + right.factor) - newLeftFactor
            in
            ( { left | factor = newLeftFactor }, { right | factor = newRightFactor } )

        newVerticalSplitterPosition : Offset -> DockingArea -> Int -> ( DockingItem, DockingItem ) -> ( DockingItem, DockingItem )
        newVerticalSplitterPosition mouseOffset area index ( upper, lower ) =
            let
                upperY =
                    area.pos.y - model.toolbar.height + area.size.height * (List.take index area.items |> List.foldl (\item -> (+) item.factor) 0)

                -- splitter 上にあるペインの高さ
                upperHeight =
                    area.size.height * upper.factor

                -- splitter の下にあるペインの高さ
                lowerHeight =
                    area.size.height * lower.factor

                -- マウスで移動できる範囲のマージン
                margin =
                    20

                -- 移動できる範囲に制限したX座標
                trimmedY =
                    max (upperY + margin) (min (upperY + upperHeight + lowerHeight - margin) (model.mousePos.y - mouseOffset.offsetY))

                newUpperFactor =
                    (trimmedY - upperY) / (upperHeight + lowerHeight) * (upper.factor + lower.factor)

                newLowerFactor =
                    (upper.factor + lower.factor) - newUpperFactor
            in
            ( { upper | factor = newUpperFactor }, { lower | factor = newLowerFactor } )

        updateMouseHoverEvent : MouseHoverEvent a -> (HoverStatus a -> b) -> b
        updateMouseHoverEvent event updater =
            case event of
                OnMouseOver id ->
                    updater (Hovered id)

                OnMouseOut _ ->
                    updater NotHovered

        -- ツールバーに関するマウスイベントを処理する
        updateToolbarMouseEvent : ToolbarHoverEvent -> Toolbar -> Toolbar
        updateToolbarMouseEvent event toolbar =
            updateMouseHoverEvent event (\status -> { toolbar | hoverStatus = status })

        -- スプリッターに関するマウスイベントを処理する
        updateSplitterMouseEvent : SplitterHoverEvent -> Model -> Model
        updateSplitterMouseEvent event model0 =
            updateMouseHoverEvent event
                (\status -> { model0 | splitterHoverStatus = status })

        updateMarkerMouseEvent : MarkerMouseEvent -> Model -> Model
        updateMarkerMouseEvent event model0 =
            updateMouseHoverEvent event
                (\status -> { model0 | markerHoverStatus = status })

        -- ペインを追加する
        addNewPane : Model -> Model
        addNewPane model0 =
            let
                pane =
                    createPane model.nextPaneId
            in
            { model0
                | nextPaneId = model.nextPaneId + 1
                , paneDict = Dict.insert model.nextPaneId pane model.paneDict
                , dockingAreaDict = insertPane pane model.dockingRoot model.dockingAreaDict
            }

        addPaneToDockingRoot : PaneId -> Model -> Model
        addPaneToDockingRoot paneId model0 =
            case Dict.get paneId model.paneDict of
                Nothing ->
                    model0

                Just pane ->
                    let
                        updatedPane =
                            { pane | status = Docked model.dockingRoot }
                    in
                    { model0
                        | paneDict = Dict.insert paneId updatedPane model.paneDict
                        , dockingAreaDict = insertPane updatedPane model.dockingRoot model.dockingAreaDict
                        , floatings = List.filter ((/=) paneId) model.floatings
                        , markerHoverStatus = NotHovered
                    }

        insertPaneToMarkerPos : DockingArea -> PaneId -> Pane -> Int -> Model -> Model
        insertPaneToMarkerPos area dockingPaneId pane offset model0 =
            let
                index =
                    Maybe.withDefault -1 <|
                        List.Extra.findIndex (\item -> item.itemType == PaneItem dockingPaneId) area.items

                newArea =
                    insertItemToDockingArea (PaneItem pane.id) (offset + index) area
            in
            { model0
                | paneDict = Dict.insert pane.id { pane | status = Docked area.id } model0.paneDict
                , dockingAreaDict = Dict.insert area.id newArea model0.dockingAreaDict
                , floatings = List.filter ((/=) pane.id) model.floatings
                , markerHoverStatus = NotHovered
            }

        splitPane : DockingArea -> Pane -> Pane -> Orientation -> Int -> Model -> Model
        splitPane area dockingPane pane orientation offset model0 =
            let
                newArea =
                    insertItemToDockingArea
                        (PaneItem pane.id)
                        offset
                        { id = model0.nextDockingAreaId
                        , pos = pane.pos
                        , size = pane.size
                        , orientation = orientation
                        , items = [ { itemType = PaneItem dockingPane.id, factor = 1.0 } ]
                        }

                updatedArea =
                    { area
                        | items =
                            List.map
                                (\item ->
                                    if item.itemType == PaneItem dockingPane.id then
                                        { item | itemType = DockingAreaItem newArea.id }

                                    else
                                        item
                                )
                                area.items
                    }
            in
            { model0
                | nextDockingAreaId = model0.nextDockingAreaId + 1
                , paneDict =
                    Dict.insert pane.id { pane | status = Docked newArea.id } <|
                        Dict.insert dockingPane.id { dockingPane | status = Docked newArea.id } <|
                            model0.paneDict
                , dockingAreaDict =
                    Dict.insert newArea.id newArea <|
                        Dict.insert updatedArea.id updatedArea <|
                            model0.dockingAreaDict
                , floatings = List.filter ((/=) pane.id) model.floatings
                , markerHoverStatus = NotHovered
            }

        insertItemToDockingArea : DockingItemType -> Int -> DockingArea -> DockingArea
        insertItemToDockingArea itemType index area =
            let
                getAt : Int -> Maybe DockingItem
                getAt idx =
                    List.Extra.getAt idx area.items

                setAt : Int -> DockingItem -> List DockingItem -> List DockingItem
                setAt =
                    List.Extra.setAt

                insertAt : Int -> DockingItem -> List DockingItem -> List DockingItem
                insertAt idx item list =
                    List.Extra.splitAt idx list |> (\( lower, upper ) -> lower ++ (item :: upper))
            in
            case ( getAt (index - 1), getAt index ) of
                ( Nothing, Nothing ) ->
                    area

                ( Nothing, Just item ) ->
                    { area
                        | items = { itemType = itemType, factor = item.factor / 2 } :: setAt index { item | factor = item.factor / 2 } area.items
                    }

                ( Just item, Nothing ) ->
                    { area
                        | items =
                            setAt (index - 1) { item | factor = item.factor / 2 } area.items
                                ++ [ { itemType = itemType, factor = item.factor / 2 } ]
                    }

                ( Just item1, Just item2 ) ->
                    { area
                        | items =
                            insertAt index { itemType = itemType, factor = item1.factor / 3 + item2.factor / 3 } <|
                                setAt (index - 1) { item1 | factor = item1.factor / 3 * 2 } <|
                                    setAt index { item2 | factor = item2.factor / 3 * 2 } area.items
                    }

        updatePaneArrowMarkerEvent : ArrowMarker -> PaneId -> PaneId -> Model -> Model
        updatePaneArrowMarkerEvent arrow dockedPaneId paneId model0 =
            case getPaneDockingArea dockedPaneId model0 of
                Just area ->
                    updateArrowMarkerEvent arrow area dockedPaneId paneId model0

                Nothing ->
                    model0

        updateRootArrowMarkerEvent : ArrowMarker -> PaneId -> Model -> Model
        updateRootArrowMarkerEvent arrow paneId model0 =
            let
                insertExisting : DockingArea -> Pane -> Int -> Model
                insertExisting area pane index =
                    { model0
                        | dockingAreaDict = Dict.insert area.id (insertItemToDockingArea (PaneItem paneId) index area) model0.dockingAreaDict
                        , paneDict = Dict.insert pane.id { pane | status = Docked area.id } model0.paneDict
                        , floatings = List.filter ((/=) pane.id) model.floatings
                        , markerHoverStatus = NotHovered
                    }

                createNewRoot : DockingArea -> Pane -> Orientation -> Int -> Model
                createNewRoot area pane orientation index =
                    let
                        newRootArea =
                            insertItemToDockingArea
                                (PaneItem paneId)
                                index
                                { area
                                    | id = model0.nextDockingAreaId
                                    , orientation = orientation
                                    , items = [ { itemType = DockingAreaItem model0.dockingRoot, factor = 1.0 } ]
                                }
                    in
                    { model0
                        | nextDockingAreaId = model0.nextDockingAreaId + 1
                        , dockingRoot = model0.nextDockingAreaId
                        , dockingAreaDict = Dict.insert newRootArea.id newRootArea model0.dockingAreaDict
                        , paneDict = Dict.insert pane.id { pane | status = Docked newRootArea.id } model0.paneDict
                        , floatings = List.filter ((/=) pane.id) model.floatings
                        , markerHoverStatus = NotHovered
                    }
            in
            case ( Dict.get model0.dockingRoot model0.dockingAreaDict, Dict.get paneId model0.paneDict ) of
                ( Just area, Just pane ) ->
                    case ( area.orientation, arrow ) of
                        ( Horizontal, LeftMarker ) ->
                            insertExisting area pane 0

                        ( Horizontal, RightMarker ) ->
                            insertExisting area pane (List.length area.items)

                        ( Horizontal, UpMarker ) ->
                            createNewRoot area pane Vertical 0

                        ( Horizontal, DownMarker ) ->
                            createNewRoot area pane Vertical 1

                        ( Vertical, LeftMarker ) ->
                            createNewRoot area pane Horizontal 0

                        ( Vertical, RightMarker ) ->
                            createNewRoot area pane Horizontal 1

                        ( Vertical, UpMarker ) ->
                            insertExisting area pane 0

                        ( Vertical, DownMarker ) ->
                            insertExisting area pane (List.length area.items)

                _ ->
                    model0

        updateArrowMarkerEvent : ArrowMarker -> DockingArea -> PaneId -> PaneId -> Model -> Model
        updateArrowMarkerEvent arrow area dockedPaneId paneId model0 =
            case ( Dict.get dockedPaneId model0.paneDict, Dict.get paneId model0.paneDict ) of
                ( Just dockedPane, Just pane ) ->
                    case ( area.orientation, arrow ) of
                        ( Horizontal, LeftMarker ) ->
                            insertPaneToMarkerPos area dockedPane.id pane 0 model0

                        ( Horizontal, RightMarker ) ->
                            insertPaneToMarkerPos area dockedPane.id pane 1 model0

                        ( Horizontal, UpMarker ) ->
                            splitPane area dockedPane pane Vertical 0 model0

                        ( Horizontal, DownMarker ) ->
                            splitPane area dockedPane pane Vertical 1 model0

                        ( Vertical, LeftMarker ) ->
                            splitPane area dockedPane pane Horizontal 0 model0

                        ( Vertical, RightMarker ) ->
                            splitPane area dockedPane pane Horizontal 1 model0

                        ( Vertical, UpMarker ) ->
                            insertPaneToMarkerPos area dockedPane.id pane 0 model0

                        ( Vertical, DownMarker ) ->
                            insertPaneToMarkerPos area dockedPane.id pane 1 model0

                _ ->
                    model0

        getPaneDockingArea : PaneId -> Model -> Maybe DockingArea
        getPaneDockingArea dockedPaneId model0 =
            Maybe.andThen
                (\pane ->
                    case pane.status of
                        Floating ->
                            Nothing

                        Docked areaId ->
                            Dict.get areaId model0.dockingAreaDict
                )
                (Dict.get dockedPaneId model0.paneDict)

        -- 新しいペインを作成する
        createPane : PaneId -> Pane
        createPane paneId =
            { id = paneId
            , title = "Pane " ++ iStr paneId
            , pos = { x = 0, y = 0 }
            , size = { width = 50, height = 50 }
            , status = Docked model.dockingRoot
            }

        -- 指定した id の DockingArea の先頭にペインを追加する
        insertPane : Pane -> PaneId -> DockingAreaDict -> DockingAreaDict
        insertPane pane id dict =
            case Dict.get id dict of
                Nothing ->
                    dict

                Just area ->
                    Dict.insert id (insertPaneToArea pane area) dict

        -- DockingArea の先頭にペインを追加する
        insertPaneToArea : Pane -> DockingArea -> DockingArea
        insertPaneToArea pane area =
            case area.items of
                [] ->
                    { area | items = [ { itemType = PaneItem pane.id, factor = 1.0 } ] }

                head :: rest ->
                    { area
                        | items =
                            { itemType = PaneItem pane.id, factor = head.factor / 2 } :: { head | factor = head.factor / 2 } :: rest
                    }

        floatingsMoveToTop : PaneId -> List PaneId -> List PaneId
        floatingsMoveToTop id floatings =
            List.partition ((==) id) floatings |> (\( left, right ) -> left ++ right)
    in
    case msg of
        BrowserResize winSize ->
            withCmdNone <| updateRootDockingLayout { model | winSize = winSize }

        MouseMove mousePos ->
            updateMouseMoveEvent { model | mousePos = mousePos }

        MouseUp _ ->
            withCmdNone { model | draggingStatus = NotDragging }

        ToolbarHoverEvent event ->
            withCmdNone { model | toolbar = updateToolbarMouseEvent event model.toolbar }

        SplitterHoverEvent event ->
            withCmdNone <| updateSplitterMouseEvent event model

        StartSplitterDragging splitterId ->
            withCmdNone { model | draggingStatus = DraggingSplitter splitterId }

        MarkerMouseEvent event ->
            withCmdNone <| updateMarkerMouseEvent event model

        AddNewPane ->
            withCmdNone <| updateRootDockingLayout (addNewPane model)

        PaneTitleMouseDown id ->
            case Dict.get id model.paneDict of
                Nothing ->
                    withCmdNone model

                Just pane ->
                    withCmdNone
                        { model
                            | draggingStatus = DraggingPane pane (subPos model.mousePos pane.pos)
                            , floatings = floatingsMoveToTop id model.floatings
                        }

        PaneBodyMouseDown id ->
            withCmdNone { model | floatings = floatingsMoveToTop id model.floatings }

        DockPane RootCenterMarker paneId ->
            withCmdNone <|
                updateRootDockingLayout <|
                    addPaneToDockingRoot paneId model

        DockPane (RootArrowMarker arrow) paneId ->
            withCmdNone <|
                updateRootDockingLayout <|
                    updateRootArrowMarkerEvent arrow paneId model

        DockPane (PaneArrowMarker arrow dockedPaneId) paneId ->
            withCmdNone <|
                updateRootDockingLayout <|
                    updatePaneArrowMarkerEvent arrow dockedPaneId paneId model

        ResizeHandleMouseDown id ->
            case Dict.get id model.paneDict of
                Nothing ->
                    withCmdNone model

                Just pane ->
                    withCmdNone
                        { model
                            | draggingStatus = ResizingPane pane
                            , floatings = floatingsMoveToTop id model.floatings
                        }

        CloseButtonMouseDown id ->
            case getPaneDockingArea id model of
                Nothing ->
                    withCmdNone <|
                        updateRootDockingLayout
                            { model
                                | paneDict = Dict.remove id model.paneDict
                                , floatings = List.filter ((/=) id) model.floatings
                            }

                Just area ->
                    withCmdNone <|
                        updateRootDockingLayout
                            { model
                                | dockingAreaDict = Dict.insert area.id (removeDockingAreaPane (PaneItem id) area) model.dockingAreaDict
                                , paneDict = Dict.remove id model.paneDict
                            }


removeDockingAreaPane : DockingItemType -> DockingArea -> DockingArea
removeDockingAreaPane itemType area =
    let
        paneIndex : Int
        paneIndex =
            Maybe.withDefault -1 <|
                List.Extra.findIndex (\item -> item.itemType == itemType) area.items

        getAt : Int -> Maybe DockingItem
        getAt idx =
            List.Extra.getAt idx area.items

        setAt : Int -> DockingItem -> List DockingItem -> List DockingItem
        setAt =
            List.Extra.setAt

        removeAt : Int -> List DockingItem -> List DockingItem
        removeAt =
            List.Extra.removeAt
    in
    case ( getAt (paneIndex - 1), getAt paneIndex, getAt (paneIndex + 1) ) of
        ( Nothing, Just _, Nothing ) ->
            { area | items = [] }

        ( Just left, Just { factor }, Nothing ) ->
            { area
                | items =
                    removeAt paneIndex <|
                        setAt (paneIndex - 1) { left | factor = left.factor + factor } area.items
            }

        ( Nothing, Just { factor }, Just right ) ->
            { area
                | items =
                    removeAt paneIndex <|
                        setAt (paneIndex + 1) { right | factor = right.factor + factor } area.items
            }

        ( Just left, Just { factor }, Just right ) ->
            { area
                | items =
                    removeAt paneIndex <|
                        setAt (paneIndex - 1) { left | factor = left.factor + factor / 2 } <|
                            setAt (paneIndex + 1) { right | factor = right.factor + factor / 2 } area.items
            }

        _ ->
            area


removeEmptyDockingArea : DockingArea -> Model -> Model
removeEmptyDockingArea targetArea model0 =
    let
        step : DockingAreaId -> DockingArea -> DockingAreaDict -> DockingAreaDict
        step id area dict =
            if List.any (\item -> item.itemType == DockingAreaItem targetArea.id) area.items then
                Dict.insert id (removeDockingAreaPane (DockingAreaItem targetArea.id) area) dict

            else
                Dict.insert id area dict
    in
    if List.length targetArea.items == 0 then
        { model0 | dockingAreaDict = Dict.foldl step Dict.empty model0.dockingAreaDict }

    else
        model0


updateRootDockingLayout : Model -> Model
updateRootDockingLayout model =
    let
        newModel =
            updateDockingLayout
                model.dockingRoot
                { x = 0, y = model.toolbar.height }
                { width = model.winSize.width, height = model.winSize.height - model.toolbar.height }
                model
    in
    { newModel | dockingAreaDict = Dict.filter (\id area -> id == model.dockingRoot || area.items /= []) newModel.dockingAreaDict }


updateDockingLayout : DockingAreaId -> Pos -> Size -> Model -> Model
updateDockingLayout id pos size model =
    let
        margin : Float
        margin =
            4

        horizontalLayout : List DockingItem -> Float -> Model -> Model
        horizontalLayout list x model0 =
            case list of
                [] ->
                    model0

                item :: rest ->
                    let
                        paneWidth : Float
                        paneWidth =
                            size.width * item.factor

                        paneFunc : Pane -> Pane
                        paneFunc pane =
                            { pane
                                | pos = { x = x + margin, y = pos.y + margin }
                                , size = { width = paneWidth - margin * 2, height = size.height - margin * 2 }
                            }
                    in
                    case item.itemType of
                        PaneItem paneId ->
                            horizontalLayout rest
                                (x + paneWidth)
                                { model0 | paneDict = Dict.update paneId (Maybe.map paneFunc) model0.paneDict }

                        DockingAreaItem areaId ->
                            horizontalLayout rest
                                (x + paneWidth)
                                (updateDockingLayout areaId { x = x, y = pos.y } { width = paneWidth, height = size.height } model0)

        verticalLayout : List DockingItem -> Float -> Model -> Model
        verticalLayout list y model0 =
            case list of
                [] ->
                    model0

                item :: rest ->
                    let
                        paneHeight : Float
                        paneHeight =
                            size.height * item.factor

                        paneFunc : Pane -> Pane
                        paneFunc pane =
                            { pane
                                | pos = { x = pos.x + margin, y = y + margin }
                                , size = { width = size.width - margin * 2, height = paneHeight - margin * 2 }
                            }
                    in
                    case item.itemType of
                        PaneItem paneId ->
                            verticalLayout rest
                                (y + paneHeight)
                                { model0 | paneDict = Dict.update paneId (Maybe.map paneFunc) model0.paneDict }

                        DockingAreaItem areaId ->
                            verticalLayout rest
                                (y + paneHeight)
                                (updateDockingLayout areaId { x = pos.x, y = y } { width = size.width, height = paneHeight } model0)
    in
    case Dict.get id model.dockingAreaDict of
        Nothing ->
            model

        Just area ->
            let
                newModel =
                    { model | dockingAreaDict = Dict.insert id { area | size = size, pos = pos } model.dockingAreaDict }
            in
            case area.orientation of
                Horizontal ->
                    if model.dockingRoot /= area.id && List.length area.items == 0 then
                        updateRootDockingLayout <| removeEmptyDockingArea area newModel

                    else
                        horizontalLayout area.items pos.x newModel

                Vertical ->
                    if model.dockingRoot /= area.id && List.length area.items == 0 then
                        updateRootDockingLayout <| removeEmptyDockingArea area newModel

                    else
                        verticalLayout area.items pos.y newModel



-- VIEW


hslString : Int -> Int -> Int -> String
hslString hue saturation lightness =
    "hsl(" ++ iStr hue ++ "," ++ iStr saturation ++ "%," ++ iStr lightness ++ "%)"


translateString : Float -> Float -> String
translateString x y =
    "translate(" ++ fStr x ++ "," ++ fStr y ++ ")"


svgMouseEvent : String -> msg -> Svg.Attribute msg
svgMouseEvent eventName msg =
    Svg.Events.custom eventName <|
        Dec.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = False
            }


svgMousedown : msg -> Svg.Attribute msg
svgMousedown =
    svgMouseEvent "mousedown"


svgMouseup : msg -> Svg.Attribute msg
svgMouseup =
    svgMouseEvent "mouseup"


sizeAttr : Size -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
sizeAttr { width, height } rest =
    Attr.width (fStr width) :: Attr.height (fStr height) :: rest


posAttr : Pos -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
posAttr { x, y } rest =
    Attr.x (fStr x) :: Attr.y (fStr y) :: rest


type alias Icon msg =
    Color.Color -> Int -> Svg msg


type alias ToolId =
    Int


type alias Tool =
    { id : ToolId
    , icon : Icon Msg
    , msg : Msg
    }


toolbarSvg : Model -> List Tool -> Svg Msg
toolbarSvg model tools =
    let
        margin : Float
        margin =
            4

        buttonSize : Float
        buttonSize =
            model.toolbar.height - margin * 2

        buttonIconSize : Float
        buttonIconSize =
            buttonSize - margin * 2

        buttonColor : Int -> String
        buttonColor id =
            case model.toolbar.hoverStatus of
                NotHovered ->
                    backgroundColor

                Hovered targetId ->
                    if id == targetId then
                        hslString 0 0 90

                    else
                        backgroundColor

        buttonSvg : Float -> Tool -> Svg Msg
        buttonSvg x { icon, id, msg } =
            Svg.g
                [ Attr.transform (translateString (x + margin) margin) ]
                [ Svg.rect
                    (sizeAttr { width = buttonSize, height = buttonSize }
                        [ Attr.fill (buttonColor id)
                        , Attr.rx "3"
                        , Attr.cursor "pointer"
                        , Svg.Events.onMouseOver (ToolbarHoverEvent (OnMouseOver id))
                        , Svg.Events.onMouseOut (ToolbarHoverEvent (OnMouseOut id))
                        , svgMousedown msg
                        , svgMouseup (ToolbarHoverEvent (OnMouseOver id))
                        ]
                    )
                    []
                , Svg.g
                    [ Attr.transform ("translate(" ++ fStr margin ++ "," ++ fStr margin ++ ")")
                    , Attr.pointerEvents "none"
                    ]
                    [ icon Color.black (floor buttonIconSize) ]
                ]

        buttons : List (Svg Msg)
        buttons =
            let
                step tool ( list, x ) =
                    ( buttonSvg x tool :: list, x + buttonSize + margin )
            in
            Tuple.first <| List.foldl step ( [], 0 ) tools

        backgroundColor : String
        backgroundColor =
            hslString 0 0 80

        background : Svg Msg
        background =
            Svg.rect
                (posAttr { x = 0, y = 0 } <|
                    sizeAttr { width = model.winSize.width, height = model.toolbar.height } <|
                        [ Attr.fill backgroundColor ]
                )
                []
    in
    Svg.g [] (background :: buttons)


paneSvg : Model -> Pane -> Svg Msg
paneSvg model pane =
    let
        titleHeight : Float
        titleHeight =
            24

        titleBackgroundColor : String
        titleBackgroundColor =
            case model.draggingStatus of
                DraggingPane targetPane _ ->
                    if targetPane.id == pane.id then
                        hslString 0 20 60

                    else
                        hslString 0 0 60

                _ ->
                    hslString 0 0 60

        titleTextColor : String
        titleTextColor =
            hslString 0 0 0

        titleTextFontSize : Float
        titleTextFontSize =
            titleHeight / 2

        frameWidth : Float
        frameWidth =
            2

        shadowAttr : List (Svg.Attribute Msg)
        shadowAttr =
            if pane.status == Floating then
                [ Attr.style "filter: url(#drop-shadow);" ]

            else
                []

        background : Svg Msg
        background =
            Svg.rect
                (posAttr pane.pos <|
                    sizeAttr pane.size <|
                        [ Attr.fill "white"
                        , Attr.stroke titleBackgroundColor
                        , Attr.strokeWidth (fStr frameWidth)
                        , svgMousedown (PaneBodyMouseDown pane.id)
                        ]
                            ++ shadowAttr
                )
                []

        title : Svg Msg
        title =
            Svg.rect
                (posAttr pane.pos <|
                    sizeAttr { width = pane.size.width, height = titleHeight } <|
                        [ Attr.fill titleBackgroundColor
                        , svgMousedown (PaneTitleMouseDown pane.id)
                        ]
                )
                []

        closeButtonMargin : Float
        closeButtonMargin =
            4

        closeButtonSize : Float
        closeButtonSize =
            titleHeight - closeButtonMargin * 2

        closeButtonColor : String
        closeButtonColor =
            hslString 0 0 40

        closeButtonTranslate =
            translateString
                (pane.pos.x + pane.size.width - closeButtonSize - closeButtonMargin)
                (pane.pos.y + closeButtonMargin)

        closeButton : Svg Msg
        closeButton =
            Svg.g [ Attr.transform closeButtonTranslate ]
                [ Svg.rect
                    (sizeAttr { width = closeButtonSize, height = closeButtonSize } <|
                        [ Attr.fill closeButtonColor
                        , svgMousedown (CloseButtonMouseDown pane.id)
                        ]
                    )
                    []
                ]

        titleText : Svg Msg
        titleText =
            Svg.text_
                (posAttr { x = pane.pos.x + 5, y = pane.pos.y + titleHeight / 2 }
                    [ Attr.fill titleTextColor
                    , Attr.fontSize (String.fromFloat titleTextFontSize)
                    , Attr.dominantBaseline "middle"
                    , Attr.pointerEvents "none"
                    ]
                )
                [ Svg.text pane.title ]

        resizeHandleColor : String
        resizeHandleColor =
            hslString 0 0 60

        resizeHandle : List (Svg Msg)
        resizeHandle =
            if pane.status == Floating then
                [ Svg.polyline
                    [ Attr.transform <| translateString (pane.pos.x + pane.size.width) (pane.pos.y + pane.size.height)
                    , Attr.points "0 0, -10, 0, 0, -10"
                    , Attr.fill resizeHandleColor
                    , svgMousedown (ResizeHandleMouseDown pane.id)
                    , Attr.cursor "se-resize"
                    ]
                    []
                ]

            else
                []
    in
    Svg.g []
        ([ background, title, titleText, closeButton ] ++ resizeHandle)


dockingAreaSvg : Model -> List (Svg Msg)
dockingAreaSvg model =
    let
        defaultSplitterLineColor : String
        defaultSplitterLineColor =
            hslString 0 0 80

        activeSplitterLineColor : String
        activeSplitterLineColor =
            hslString 0 20 50

        splitterLineColor : SplitterId -> String
        splitterLineColor splitterId =
            if model.draggingStatus == DraggingSplitter splitterId || model.splitterHoverStatus == Hovered splitterId then
                activeSplitterLineColor

            else
                defaultSplitterLineColor

        splitterLineWidth =
            4

        splitterLineMargin =
            2

        splitter : SplitterId -> Pos -> Pos -> List (Svg.Attribute Msg) -> Svg Msg
        splitter splitterId pos1 pos2 attrs =
            Svg.line
                ([ Attr.stroke (splitterLineColor splitterId)
                 , Attr.strokeWidth (fStr splitterLineWidth)
                 , Attr.x1 (fStr pos1.x)
                 , Attr.y1 (fStr pos1.y)
                 , Attr.x2 (fStr pos2.x)
                 , Attr.y2 (fStr pos2.y)
                 , Svg.Events.onMouseOver (SplitterHoverEvent (OnMouseOver splitterId))
                 , Svg.Events.onMouseOut (SplitterHoverEvent (OnMouseOut splitterId))
                 , svgMousedown (StartSplitterDragging splitterId)
                 ]
                    ++ attrs
                )
                []

        splitterList : Orientation -> DockingArea -> List DockingItem -> Int -> Float -> List (Svg Msg) -> List (Svg Msg)
        splitterList orientation area items index factor list =
            case items of
                [] ->
                    list

                item :: rest ->
                    let
                        x =
                            area.pos.x + factor * area.size.width

                        y =
                            area.pos.y + factor * area.size.height

                        line =
                            case orientation of
                                Horizontal ->
                                    splitter
                                        { areaId = area.id, index = index }
                                        { x = x, y = area.pos.y + splitterLineMargin }
                                        { x = x, y = area.pos.y + area.size.height - splitterLineMargin * 2 }
                                        [ Attr.cursor "col-resize" ]

                                Vertical ->
                                    splitter
                                        { areaId = area.id, index = index }
                                        { x = area.pos.x + splitterLineMargin, y = y }
                                        { x = area.pos.x + area.size.width - splitterLineMargin * 2, y = y }
                                        [ Attr.cursor "row-resize" ]
                    in
                    splitterList orientation area rest (index + 1) (factor + item.factor) (line :: list)

        dockingAreaDictStep : Int -> DockingArea -> List (Svg Msg) -> List (Svg Msg)
        dockingAreaDictStep _ area svgList =
            case area.items of
                [] ->
                    svgList

                { factor } :: rest ->
                    splitterList area.orientation area rest 0 factor [] ++ svgList

        paneDictStep : PaneId -> Pane -> List (Svg Msg) -> List (Svg Msg)
        paneDictStep _ pane svgList =
            if pane.status == Floating then
                svgList

            else
                paneSvg model pane :: svgList
    in
    Dict.foldl paneDictStep [] model.paneDict
        ++ Dict.foldl dockingAreaDictStep [] model.dockingAreaDict


floatingAreaSvg : Model -> List (Svg Msg)
floatingAreaSvg model =
    let
        step : PaneId -> List (Svg Msg) -> List (Svg Msg)
        step paneId svgList =
            case Dict.get paneId model.paneDict of
                Nothing ->
                    svgList

                Just pane ->
                    paneSvg model pane :: svgList
    in
    List.foldl step [] model.floatings


dockingMarkerSvg : Model -> List (Svg Msg)
dockingMarkerSvg model =
    let
        markerSize : Float
        markerSize =
            28

        markerIconMargin =
            2

        markerIconSize =
            markerSize - markerIconMargin * 2

        markerFillColor isHovered =
            if isHovered then
                hslString 60 80 70

            else
                hslString 0 0 90

        markerStrokeColor =
            hslString 0 0 40

        dockingMarker : MarkerId -> PaneId -> Pos -> Icon Msg -> Svg Msg
        dockingMarker markerId draggingId pos icon =
            Svg.g [ Attr.transform (translateString (pos.x - markerSize / 2) (pos.y - markerSize / 2)) ]
                [ Svg.rect
                    (sizeAttr { width = markerSize, height = markerSize }
                        [ Attr.fill (markerFillColor (Hovered markerId == model.markerHoverStatus))
                        , Attr.stroke markerStrokeColor
                        , Attr.rx "3"
                        , Svg.Events.onMouseOver (MarkerMouseEvent (OnMouseOver markerId))
                        , Svg.Events.onMouseOut (MarkerMouseEvent (OnMouseOut markerId))
                        , Svg.Events.onMouseUp (DockPane markerId draggingId)
                        ]
                    )
                    []
                , Svg.g
                    [ Attr.transform (translateString markerIconMargin markerIconMargin)
                    , Attr.pointerEvents "none"
                    ]
                    [ icon Color.black (floor markerIconSize) ]
                ]

        noDockedPane : Bool
        noDockedPane =
            case Dict.get model.dockingRoot model.dockingAreaDict of
                Nothing ->
                    False

                Just area ->
                    List.length area.items == 0

        rootMarkerMargin =
            30

        rootDockingMarker : PaneId -> List (Svg Msg)
        rootDockingMarker draggingId =
            [ dockingMarker (RootArrowMarker LeftMarker)
                draggingId
                { x = rootMarkerMargin
                , y = model.toolbar.height + (model.winSize.height - model.toolbar.height) / 2
                }
                NavigationIcons.arrow_back
            , dockingMarker (RootArrowMarker RightMarker)
                draggingId
                { x = model.winSize.width - rootMarkerMargin
                , y = model.toolbar.height + (model.winSize.height - model.toolbar.height) / 2
                }
                NavigationIcons.arrow_forward
            , dockingMarker (RootArrowMarker UpMarker)
                draggingId
                { x = model.winSize.width / 2
                , y = rootMarkerMargin
                }
                NavigationIcons.arrow_upward
            , dockingMarker (RootArrowMarker DownMarker)
                draggingId
                { x = model.winSize.width / 2
                , y = model.winSize.height - rootMarkerMargin
                }
                NavigationIcons.arrow_downward
            ]
                ++ (if noDockedPane then
                        [ dockingMarker
                            RootCenterMarker
                            draggingId
                            { x = model.winSize.width / 2
                            , y = model.toolbar.height + (model.winSize.height - model.toolbar.height) / 2
                            }
                            ActionIcons.open_with
                        ]

                    else
                        []
                   )

        markerOffset : Float
        markerOffset =
            markerSize + 5

        paneDictStep : PaneId -> PaneId -> Pane -> List (Svg Msg) -> List (Svg Msg)
        paneDictStep draggingId paneId pane svgList =
            let
                centerX =
                    pane.pos.x + pane.size.width / 2

                centerY =
                    pane.pos.y + pane.size.height / 2
            in
            if draggingId /= paneId && pane.status /= Floating && insideArea model.mousePos ( pane.pos, pane.size ) then
                dockingMarker (PaneArrowMarker LeftMarker paneId)
                    draggingId
                    { x = centerX - markerOffset, y = centerY }
                    NavigationIcons.arrow_back
                    :: dockingMarker (PaneArrowMarker RightMarker paneId)
                        draggingId
                        { x = centerX + markerOffset, y = centerY }
                        NavigationIcons.arrow_forward
                    :: dockingMarker (PaneArrowMarker UpMarker paneId)
                        draggingId
                        { x = centerX, y = centerY - markerOffset }
                        NavigationIcons.arrow_upward
                    :: dockingMarker (PaneArrowMarker DownMarker paneId)
                        draggingId
                        { x = centerX, y = centerY + markerOffset }
                        NavigationIcons.arrow_downward
                    :: svgList

            else
                svgList
    in
    case model.draggingStatus of
        DraggingPane pane _ ->
            Dict.foldl (paneDictStep pane.id) [] model.paneDict ++ rootDockingMarker pane.id

        _ ->
            []


view : Model -> Document Msg
view model =
    let
        sizeString : Size -> String
        sizeString { width, height } =
            fStrConcat2 width height

        filterDefs : Svg msg
        filterDefs =
            Svg.defs []
                [ Svg.filter
                    [ Attr.id "drop-shadow" ]
                    [ Svg.feGaussianBlur [ Attr.in_ "SourceAlpha", Attr.stdDeviation "2", Attr.result "blur" ] []
                    , Svg.feOffset [ Attr.in_ "blur", Attr.dx "2", Attr.dy "2", Attr.result "offsetBlur" ] []
                    , Svg.feComponentTransfer
                        [ Attr.in_ "offsetBlur", Attr.result "offsetBlurScreen" ]
                        [ Svg.feFuncA [ Attr.type_ "linear", Attr.slope "0.4" ] [] ]
                    , Svg.feMerge []
                        [ Svg.feMergeNode [ Attr.in_ "offsetBlurScreen" ] []
                        , Svg.feMergeNode [ Attr.in_ "SourceGraphic" ] []
                        ]
                    ]
                ]

        backgroundColor : String
        backgroundColor =
            hslString 0 0 80

        backgroundSvg : Size -> Svg Msg
        backgroundSvg size =
            Svg.rect
                (posAttr { x = 0, y = 0 } <| sizeAttr size <| [ Attr.fill backgroundColor ])
                []

        tools : List Tool
        tools =
            [ { id = 0, msg = AddNewPane, icon = ContentIcons.add_circle_outline }
            ]

        body : List (Html Msg)
        body =
            [ Svg.svg
                [ Attr.viewBox ("0 0 " ++ sizeString model.winSize) ]
                ([ filterDefs
                 , backgroundSvg model.winSize
                 , toolbarSvg model tools
                 ]
                    ++ dockingAreaSvg model
                    ++ floatingAreaSvg model
                    ++ dockingMarkerSvg model
                )
            ]
    in
    { title = "Docking Window System"
    , body = body
    }



-- UTILS


withCmdNone : a -> ( a, Cmd msg )
withCmdNone value =
    ( value, Cmd.none )


iStr : Int -> String
iStr =
    String.fromInt


fStr : Float -> String
fStr =
    String.fromFloat


fStrConcat2 : Float -> Float -> String
fStrConcat2 v1 v2 =
    fStr v1 ++ " " ++ fStr v2


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Size =
    { width : Float
    , height : Float
    }


type alias Offset =
    { offsetX : Float
    , offsetY : Float
    }


addPos : Pos -> Offset -> Pos
addPos { x, y } { offsetX, offsetY } =
    { x = x + offsetX
    , y = y + offsetY
    }


subPos : Pos -> Pos -> Offset
subPos pos1 pos2 =
    { offsetX = pos1.x - pos2.x
    , offsetY = pos1.y - pos2.y
    }


negOffset : Offset -> Offset
negOffset { offsetX, offsetY } =
    { offsetX = negate offsetX
    , offsetY = negate offsetY
    }


offsetSize : Offset -> Size
offsetSize { offsetX, offsetY } =
    { width = offsetX
    , height = offsetY
    }


distance : Pos -> Pos -> Float
distance pos1 pos2 =
    subPos pos1 pos2
        |> (\{ offsetX, offsetY } -> sqrt (offsetX ^ 2 + offsetY ^ 2))


insideArea : Pos -> ( Pos, Size ) -> Bool
insideArea { x, y } ( pos, size ) =
    x >= pos.x && x <= pos.x + size.width && y >= pos.y && y <= pos.y + size.height
