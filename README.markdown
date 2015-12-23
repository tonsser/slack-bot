# Slack Bot

A Slack bot written in Haskell

## Adding new actions

Look at how its done in `app/BotAction.hs`.

**TODO**: Explain this a bit better.

## Setup

- Create new a Slack Application. The redirect URI should be `{app url}/slack/auth/callback`.
- Configure an outgoing Slack webhook to POST to `{app url}`.
- Configure an incoming Slack webhook. Update `slackUrl` in `app/BotResponder` to have the correct webhook URL.
- Copy `.env.sample` to `.env` and fill in your `TONSS_SLACK_CLIENT_ID` and `TONSS_SLACK_CLIENT_SECRET`. This is required for testing oauth flow locally.

## Installing locally

- Install [Stack](https://github.com/commercialhaskell/stack)
- `stack setup`
- `stack build`

## Developing locally

`stack exec -- yesod devel`

You can test commands by going to `http://localhost:3000/local`

## Building

`stack build`

## Deploying

`git push heroku master`
