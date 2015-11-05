require "rest-client"
require "pp"

pp RestClient.get(
  "https://slack.com/api/users.list",
  token: "xxx",
)

# def do_it(command)
#   puts JSON.parse(
#     RestClient.post(
#       "http://localhost:3000",
#       token: "XXXXXXXXXXXXXXXXXX",
#       team_id: "T0001",
#       team_domain: "example",
#       channel_id: "C2147483705",
#       channel_name: "test",
#       timestamp: "1355517523.000005",
#       user_id: "U2147483697",
#       user_name: "Steve",
#       text: "bot #{command}",
#       trigger_word: "bot",
#     )
#   )["text"]
# end

# if ARGV.count > 0
#   do_it(ARGV.join(" "))
# else
#   do_it("help")
# end
