module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput, withLargeInput)
import Text.Julius (RawJS (..))

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    (commentFormWidget, commentFormEnctype) <- generateFormPost commentForm

    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text

    let userComment = Nothing :: Maybe MyComment
        commentText = ""
        -- commentText = myMessage userComment

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    ((commentResult, commentFormWidget), commentFormEnctype) <- runFormPost commentForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
        userComment = case (commentResult :: FormResult MyComment) of
          FormSuccess res -> do
            Just res
          _ -> Nothing
        commentText = myMessage userComment

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing


-- data UserComment = UserComment { commentText :: Text } deriving Show

commentAForm :: AForm Handler MyComment
commentAForm = MyComment
    <$> areq textField "comment-text" Nothing 

commentForm :: Html -> MForm Handler (FormResult MyComment, Widget)
commentForm = renderBootstrap3 BootstrapBasicForm commentAForm

-- commentForm :: Form Text
-- commentForm = renderBootstrap3 BootstrapBasicForm (areq textField (withLargeInput "What do you have to say?") Nothing)


commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
