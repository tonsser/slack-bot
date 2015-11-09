# Tonss

A Slack written in Haskell

## Adding new actions

Look at how its done in `app/BotAction.hs`.

**TODO**: Explain this a bit better.

## Setup

- Create new a Slack Application. The redirect URI should be `https://warm-river-9355.herokuapp.com/slack/auth/callback`.
- Configure an outgoing Slack webhook to POST to `https://warm-river-9355.herokuapp.com`.
- Configure an incoming Slack webhook. Update `slackUrl` in `app/SlackHelpers.hs` to have the correct webhook URL.
- Set `TONSS_SLACK_CLIENT_ID` and `TONSS_SLACK_CLIENT_SECRET` in environment variables.

## Installing locally

- Install [Stack](https://github.com/commercialhaskell/stack)
- `stack setup`
- `stack build`

## Developing locally

`stack exec -- yesod devel`

## Building

`stack build`

## Deploying

`git push heroku master`
