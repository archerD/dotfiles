import Xmobar

leftTemplate :: String
leftTemplate = "<action=`gnome-pie -o 232`>%StdinReader%</action>"
centerTemplate :: String
centerTemplate = "<action=`gsimplecal`><fc=#ee9a00>%date%</fc></action>"
rightTemplate :: String
rightTemplate = "<action=`playerctl -p playerctld play-pause` button=1>\
                    \<action=`playerctl -p playerctld next` button=5>\
                        \<action=`playerctl -p playerctld previous` button=4>\
                            \<action=`gnome-pie -o 353` button=3>\
                                \<fc=#44cc44>%mpris2%</fc> <fn=1>%playerstatus%</fn>\
                            \</action>\
                        \</action>\
                    \</action>\
                \</action>\
                \ [<fc=#ffff00>%locks%</fc>] %cpu% | %memory% (%default:Master%)|%trayerpad%"

config :: Config
config = defaultConfig
       { font = "xft:Tlwg Mono:size=12:bold:antialias=true"
       , additionalFonts = ["xft:Monospace Regular:size=10:antialias=true"]
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , commands = [ Run $ Cpu ["-L","25","-H","75","--normal","green","--high","red"] 10
                    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                    , Run $ Date "(%a) %F T %R:%S%z (%Z)" "date" 10
                    , Run $ StdinReader
                    , Run $ Locks
                    , Run $ Com "/home/archerd/.dotfiles/padding-icon.sh" ["panel"] "trayerpad" 10
                    , Run $ Volume "default" "Master" ["-t", "Vol: <volume>%"] 5
                    , Run $ Mpris2 "playerctld" ["-T", "41", "-E", "...", "-M", "25", "-e", "..."] 10
                    , Run $ Com "playerctl" ["-f", "{{emoji(status)}}", "status"] "playerstatus" 10
                    ]
       , sepChar = "%"
       , alignSep = "{}"
       , template = unwords [leftTemplate, "{", centerTemplate, "}", rightTemplate]
       }
main :: IO ()
main = xmobar config
