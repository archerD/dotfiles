import Xmobar
import HomeManagerProvided as HMP

leftTemplate :: String
-- leftTemplate = "<action=`gnome-pie -o 232` button=3>%XMonadLog%</action>"
leftTemplate = "<action=`rofi -show drun` button=3>%UnsafeXMonadLog%</action>"
centerTemplate :: String
centerTemplate = "<action=`gsimplecal`><fc=#ee9a00>%date%</fc></action>"
rightTemplate :: String
rightTemplate = "<action=`playerctl -p playerctld play-pause` button=1>\
                    \<action=`playerctld shift` button=3>\
                        \<action=`playerctl -p playerctld next` button=5>\
                            \<action=`playerctl -p playerctld previous` button=4>\
                                \<fc=#44cc44>%mpris2%</fc>\
                            \</action>\
                        \</action>\
                        \ <fn=1>%playerstatus%</fn>\
                    \</action>\
                \</action>\
                \ [<fc=#ffff00>%locks%</fc>] (%cpu%|%memory%|%alsa:default:Master%) <box type=Left>%trayerpad%</box>"

config :: Config
config = defaultConfig
       { font = "JetBrains Mono Nerd Font Medium " ++ HMP.fontSize
       , additionalFonts = zipWith (++) ["Monospace Regular "] HMP.additionalFontSizes
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopSize L 100 HMP.topSizeHeight
       , commands = [ Run $ Cpu ["-L","25","-H","75","--normal","green","--high","red", "--ppad","2", "-t","C:<total>%"] 10
                    , Run $ Memory ["-t","M:<usedratio>%"] 10
                    , Run $ Date "   (%a) %F T %T%z (%Z)" "date" 10
                    -- , Run   XMonadLog
                    , Run   UnsafeXMonadLog
                    , Run   Locks
                    , Run $ Com "/home/archerd/.dotfiles/padding-icon.sh" ["panel"] "trayerpad" 10
                    , Run $ Alsa "default" "Master" ["-t", "V:<volume>%"]
                    , Run $ Mpris2 "playerctld" ["-T", "38", "-E", "…", "-M", "25", "-e", ">", "-t", "<artist>/<title>"] 10
                    -- , Run $ MarqueePipeReader "/tmp/.volume-pipe" (32, 5, "|") "playerinfo"
                    -- this relies on the following running: playerctl -F -p playerctld -f "{{artist}} / {{title}}" metadata > /tmp/.volume-pipe
                    -- the pipe can be created by running _volume_pipe=/tmp/.volume-pipe; [[ -S $_volume_pipe ]] || mkfifo $_volume_pipe

                    , Run $ ComX "playerctl" ["-f", "{{emoji(status)}}", "status", "-p", "playerctld"] "X" "playerstatus" 10
                    ]
       , sepChar = "%"
       , alignSep = "{}"
       , template = unwords [leftTemplate, "{", centerTemplate, "}", rightTemplate]
       }
main :: IO ()
main = xmobar config
