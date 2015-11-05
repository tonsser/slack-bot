module Handler.SlackAuth where

import Import

getSlackAuthR :: Handler Html
getSlackAuthR =
    -- redirect to https://slack.com/oauth/authorize
    --
    -- With params:
    -- client_id    - issued when you created your app (required)
    -- scope        - permissions to request (see below) (required)
    -- redirect_uri - URL to redirect back to (see below) (optional)
    -- state        - unique string to be passed back upon completion
    -- team         - Slack team ID to restrict to (optional)
    --
    -- team is in get params as team_id
    --
    -- TODO: Set client_id in settings somehow
    -- TODO: Set client_secret in settings somehow
    -- TODO: Figure out which scopes I'll need
    --
    -- once the flow is over, save the user_id and given token in the
    -- database
    -- on future requests look up the token from the user_id and
    -- use that token for slack api calls
    error "Not yet implemented: getSlackAuthR"
