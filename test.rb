require "rest-client"
require "pp"

pp JSON.parse(RestClient.post(
  "http://localhost:3000",
  token: "XXXXXXXXXXXXXXXXXX",
  team_id: "T0001",
  team_domain: "example",
  channel_id: "C2147483705",
  channel_name: "test",
  timestamp: "1355517523.000005",
  user_id: "U2147483697",
  user_name: "Steve",
  text: "bot what time is it?",
  trigger_word: "bot",
))
