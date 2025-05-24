namespace NewProject

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open WebSharper.UI.Html
open WebSharper.Sitelets
open System


[<JavaScript>]
module Client =    
    type EndPoint =
        | [<EndPoint "/">] Home
        | [<EndPoint "/create">] Create
        | [<EndPoint "/open/{Id}">] Open of Id: int
        | [<EndPoint "/tags">] TagsPage
    
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    type Tag = {
        Id: int
        Title: string
        Color: string
    }

    let Tags = ListModel.Create (fun tag -> tag.Id) []
    let NextTagId = Var.Create 1

    type Note = {
        Id: int
        Title: string
        Content: string
        Subject: string
        Tags: list<Tag>
        CreatedDate: DateTime
        IsFavorite: bool
    }    
    let Notes = ListModel.Create (fun note -> note.Id) []
    let NextId = Var.Create 1
    
    // Add a sample for testing
    do 
        Notes.Add({
            Id = 0
            Title = "Első bejegyzés"
            Content = "<p>Ez egy teszt bejegyzés, amelyet a kedvenc funkció tesztelésére hoztam létre.</p>"
            Subject = "Test"
            Tags = []
            CreatedDate = DateTime.Now
            IsFavorite = false
        })

    let router = Router.Infer<EndPoint>()
    let currentPage = Router.InstallHash Home router

    type Router<'T when 'T: equality> with
        member this.LinkHash (ep: 'T) = "#" + this.Link ep

    module Pages =
        open WebSharper.UI.Html        
        let toggleNoteFavorite (noteId: int) =
            match Notes.TryFindByKey(noteId) with
            | Some note ->
                let updatedNote = { note with IsFavorite = not note.IsFavorite }
                Notes.RemoveByKey(note.Id)
                Notes.Add(updatedNote)
            | None ->
                JS.Alert("Bejegyzés nem található.")

        module TagsManager =
            let tagTitleVar = Var.Create ""
            let tagColorVar = Var.Create "#6c5ce7"

            let colorOptions = [
                "#6c5ce7", "Purple"
                "#00cec9", "Teal" 
                "#fd79a8", "Pink"
                "#e17055", "Orange"
                "#00b894", "Green"
                "#0984e3", "Blue"
                "#636e72", "Gray"
                "#2d3436", "Dark"
            ]

            let addNewTag() =
                let title = tagTitleVar.Value.Trim()
                let color = tagColorVar.Value
                
                if not (String.IsNullOrWhiteSpace(title)) then
                    let newTag = {
                        Id = NextTagId.Value
                        Title = title
                        Color = color
                    }
                    
                    Tags.Add(newTag)
                    NextTagId.Value <- NextTagId.Value + 1
                    
                    tagTitleVar.Value <- ""
                    JS.Alert("Címke létrehozva!")
                else
                    JS.Alert("Adj nevet a címkének.")

            let deleteTag tagId =
                Tags.RemoveByKey(tagId)

            let renderTags() =
                Tags.View.DocSeqCached(fun tag ->
                    div [attr.``class`` "tag-item"] [
                        div [attr.``class`` "color-preview"; 
                             Attr.Style "background-color" tag.Color; 
                             Attr.Style "width" "20px"; 
                             Attr.Style "height" "20px"; 
                             Attr.Style "border-radius" "50%"] []
                        span [attr.``class`` "tag-name"] [text tag.Title]
                        button [attr.``class`` "tag-delete-btn"; 
                                attr.``type`` "button";
                                on.click (fun _ _ -> deleteTag tag.Id)] [
                            i [attr.``class`` "fas fa-times"] []
                        ]
                    ]
                )

            let TagsPage() =
                IndexTemplate.TagsPage()
                    .TagName(tagTitleVar : Var<string>)
                    .TagColor(tagColorVar : Var<string>)
                    .TagsList(renderTags())
                    .ColorOptions(
                        Doc.Concat [
                            for color, name in colorOptions ->
                                let id = "color-" + name.ToLower()
                                label [attr.``class`` "color-option";
                                        attr.``for`` id] [
                                    input [attr.``type`` "radio";
                                        attr.``id`` id;
                                        attr.name "tag-color";
                                        attr.value color;
                                        on.change (fun _ _ -> tagColorVar.Value <- color)] []
                                    span [attr.``class`` "color-preview";
                                        Attr.Style "background-color" color] []
                                    text name
                                ]
                        ]
                    )
                    .CreateTag(fun _ -> addNewTag())
                    .Doc()

        module Home =
            let htmlToPlainText (html: string) =
                let tempDiv = JS.Document.CreateElement("div")
                tempDiv.InnerHTML <- html
                
                let plainText = tempDiv.TextContent
                
                if isNull plainText then "" else plainText

            let formatDate (date: DateTime) =
                let months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
                sprintf "%s %d, %d" months.[date.Month - 1] date.Day date.Year            
                
            let filterVar = Var.Create "All Notes"
            let searchTermVar = Var.Create ""
            let renderNotes () =
                let filteredNotes = 
                    View.Map3 (fun (filter: string) (searchTerm: string) notes ->
                        let initialFiltered =
                            match filter with
                            | "Favorites" -> notes |> Seq.filter (fun n -> n.IsFavorite)
                            | "Recent" -> 
                                notes 
                                |> Seq.sortByDescending (fun n -> n.CreatedDate)
                                |> Seq.truncate 5
                            | _ -> notes
                        
                        let trimmedSearch = searchTerm.Trim().ToLower()
                        if String.IsNullOrWhiteSpace(trimmedSearch) then
                            initialFiltered
                        else
                            initialFiltered |> Seq.filter (fun n ->
                                let title = n.Title.ToLower()
                                let content = (htmlToPlainText n.Content).ToLower()
                                title.Contains(trimmedSearch) || content.Contains(trimmedSearch)
                            )
                    ) filterVar.View searchTermVar.View Notes.View

                filteredNotes.DocSeqCached(fun note ->
                    IndexTemplate.NoteItem()
                        .NoteTitle(note.Title)
                        .NoteDate(formatDate note.CreatedDate)
                        .NoteContent(
                            let plainTextContent = htmlToPlainText note.Content
                            if plainTextContent.Length > 300 then 
                                plainTextContent.Substring(0, 100) + "..." 
                            else 
                                plainTextContent)
                        .NoteTags(
                            if note.Tags.IsEmpty then
                                Doc.Empty
                            else
                                Doc.Concat [
                                    for tag in note.Tags ->
                                        span [attr.``class`` "note-tag"
                                              Attr.Style "background-color" tag.Color] [
                                            text tag.Title
                                        ]
                                ]
                        )
                        .FavoriteIcon(
                            if note.IsFavorite then
                                "fas fa-star fa-lg"
                            else
                                "far fa-star fa-lg"
                        )
                        .ToggleFavorite(fun _ -> 
                            toggleNoteFavorite note.Id)
                        .OpenNote(fun _ -> 
                            currentPage.Value <- Open note.Id)
                        .DeleteNote(fun _ -> 
                            Notes.Remove(note))
                        .Doc()
                )

        module Create =
            let titleVar = Var.Create ""
            let contentVar = Var.Create ""
            let selectedTagsVar = Var.Create (Set.empty<int>)
            let toggleTag tagId =
                let currentTags = selectedTagsVar.Value
                let newTags =
                    if Set.contains tagId currentTags then
                        Set.remove tagId currentTags
                    else
                        Set.add tagId currentTags
                selectedTagsVar.Value <- newTags

            let renderSelectableTags() =
                Tags.View.DocSeqCached(fun (tag: Tag) ->
                    let isSelected = 
                        selectedTagsVar.View 
                        |> View.Map (fun selectedIds -> Set.contains tag.Id selectedIds)
                    
                    div [attr.``class`` "selectable-tag"
                         Attr.DynamicClassPred "selected" isSelected
                         Attr.DynamicStyle "background-color" (
                            isSelected |> View.Map (fun selected ->
                                if selected then tag.Color else "transparent"))
                         Attr.DynamicStyle "color" (
                            isSelected |> View.Map (fun selected ->
                                if selected then "white" else "inherit"))  
                         on.click (fun _ _ -> toggleTag tag.Id)] [
                        text tag.Title
                    ]
                )

            let initRichTextEditor() =
                let editorToolbar = JS.Document.QuerySelectorAll(".editor-toolbar .toolbar-btn")
                let editorButtons = [| for i in 0 .. editorToolbar.Length - 1 -> editorToolbar.[i] :?> HTMLElement |]
        
                for btn in editorButtons do
                    btn.AddEventListener("click", fun (e: Dom.Event) ->
                        let target = e.Target :?> HTMLElement
                        let button = 
                            if target.TagName.ToLower() = "i" then 
                                target.ParentElement :?> HTMLElement 
                            else 
                                target
                        
                        let command = button.GetAttribute("data-command")
                        
                        try
                            match command with
                            | "bold" -> JS.Document.ExecCommand("bold", false, null) |> ignore
                            | "italic" -> JS.Document.ExecCommand("italic", false, null) |> ignore
                            | "underline" -> JS.Document.ExecCommand("underline", false, null) |> ignore
                            | "createLink" -> 
                                let url = JS.Window.Prompt("Enter URL:", "https://")
                                if not (isNull url) && url <> "" then
                                    JS.Document.ExecCommand("createLink", false, url) |> ignore
                            | "insertImage" ->
                                let url = JS.Window.Prompt("Enter image URL:", "https://")
                                if not (isNull url) && url <> "" then
                                    JS.Document.ExecCommand("insertImage", false, url) |> ignore
                            | _ -> 
                                JS.Document.ExecCommand(command, false, null) |> ignore

                        with _ ->
                            Console.Log("Command execution failed: " + command)
                        
                        let editor = JS.Document.GetElementById("note-content")
                        (editor :?> HTMLElement).Focus()
                        
                        e.PreventDefault()
                    )
        
                Console.Log("Rich text editor initialized")

            let saveNote () =
                let title = titleVar.Value
                let contentElement = JS.Document.GetElementById("note-content")
                let content = contentElement.InnerHTML
                
                if not (String.IsNullOrWhiteSpace(title) || String.IsNullOrWhiteSpace(content)) then
                    let selectedTags = 
                        selectedTagsVar.Value
                        |> Set.toList
                        |> List.choose (fun tagId -> Tags.TryFindByKey tagId)
                    
                    
                    let newNote = {
                        Id = NextId.Value
                        Title = title
                        Content = content
                        Subject = "General"
                        Tags = selectedTags
                        CreatedDate = DateTime.Now
                        IsFavorite = false
                    }
                    
                    Notes.Add(newNote)
                    
                    NextId.Value <- NextId.Value + 1
                    
                    titleVar.Value <- ""
                    contentVar.Value <- ""
                    selectedTagsVar.Value <- Set.empty
                    contentElement.InnerHTML <- ""
                    
                    currentPage.Value <- Home
                    
                    JS.Alert("Bejegyzés sikeresen hozzáadva!")
                else
                    JS.Alert("Please fill out both title and content fields.")

        let HomePage() =
            IndexTemplate.HomePage()
                .NotesContainer(Home.renderNotes())                
                .FilterNotes(fun e ->
                    let select = e.Target :?> HTMLSelectElement
                    let selectedValue = select.GetAttribute("value")
                    let value = if isNull selectedValue then "All Notes" else selectedValue
                    Console.Log("Filter changed to:", value)
                    Home.filterVar.Value <- value
                )
                .CurrentFilter(Home.filterVar)
                .SearchNotes(fun e ->
                    let input = e.Target :?> HTMLInputElement
                    Console.Log("Search term entered:", input.Value)
                    Home.searchTermVar.Value <- input.Value
                )
                .SearchTerm(Home.searchTermVar)
                .CreateNewNote(fun _ -> 
                    currentPage.Value <- Create)
                .Doc()
        let CreatePage() =
            let doc = IndexTemplate.CreatePage()

            Create.selectedTagsVar.Value <- Set.empty
            
            JS.Window.SetTimeout(fun () -> 
                Create.initRichTextEditor()
                Console.Log("Rich Text Editor initialized on page load")
            , 100) |> ignore
    
            doc.SelectableTags(Create.renderSelectableTags()) |> ignore
            doc.SaveNote(fun _ ->
                let title = (JS.Document.GetElementById("note-title") :?> HTMLInputElement).Value
                let content = JS.Document.GetElementById("note-content").InnerHTML
                
                Create.titleVar.Value <- title
                Create.contentVar.Value <- content
    
                Create.saveNote()
            )
            
        let OpenPage(id: int) =
            match Notes.TryFindByKey(id) with
            | Some note ->
                let doc = IndexTemplate.CreatePage()

                let noteTagIds = note.Tags |> List.map (fun tag -> tag.Id) |> Set.ofList
                Create.selectedTagsVar.Value <- noteTagIds

                JS.Window.SetTimeout((fun () ->
                    let header = JS.Document.QuerySelector(".page-header .title-container h1")
                    if not (isNull header) then header.TextContent <- "Bejegyzés szerkesztése"
                    let subtitle = JS.Document.QuerySelector(".page-header .title-container p")
                    if not (isNull subtitle) then subtitle.TextContent <- "Szerkeszd a bejegyzést"

                    Create.initRichTextEditor()

                    let titleElement = JS.Document.GetElementById("note-title") :?> HTMLInputElement
                    let contentElement = JS.Document.GetElementById("note-content")
                    if not (isNull titleElement) then titleElement.Value <- note.Title
                    if not (isNull contentElement) then contentElement.InnerHTML <- note.Content

                    Create.titleVar.Value <- note.Title
                    Create.contentVar.Value <- note.Content
                ), 100) |> ignore

                doc.SelectableTags(Create.renderSelectableTags()) |> ignore
                doc.SaveNote(fun _ ->
                    let titleElement = JS.Document.GetElementById("note-title") :?> HTMLInputElement
                    let contentElement = JS.Document.GetElementById("note-content")
                    let title = if isNull titleElement then "" else titleElement.Value
                    let content = if isNull contentElement then "" else contentElement.InnerHTML
                    let selectedTags = 
                            Create.selectedTagsVar.Value
                            |> Set.toList
                            |> List.choose (fun tagId -> Tags.TryFindByKey tagId)
                    
                    let updatedNote = {
                        Id = note.Id
                        Title = title
                        Content = content
                        Subject = note.Subject
                        Tags = selectedTags
                        CreatedDate = note.CreatedDate
                        IsFavorite = note.IsFavorite
                    }
                    Notes.RemoveByKey(note.Id)
                    Notes.Add(updatedNote)
                    currentPage.Value <- Home
                    JS.Alert("Bejyegyzés frissítve!")
                ) |> ignore

                doc.SwitchToHome(fun _ ->
                    currentPage.Value <- Home
                ) |> ignore

                doc
            | None ->
                JS.Alert("Bejegyzés nem található.")
                currentPage.Value <- Home
                IndexTemplate.CreatePage()
            
      [<SPAEntryPoint>]
    let Main () =
        let renderInnerPage (currentPage: Var<EndPoint>) =
            currentPage.View.Map (fun endpoint ->
                match endpoint with
                | Home        -> Pages.HomePage()
                | Create      -> Pages.CreatePage().Doc()
                | Open id     -> Pages.OpenPage(id).Doc()
                | TagsPage    -> Pages.TagsManager.TagsPage()
            )
            |> Doc.EmbedView

        IndexTemplate()
            .Content(renderInnerPage currentPage)
            .SwitchToHome(fun _ ->
                currentPage.Value <- Home
            )
            .Bind()
