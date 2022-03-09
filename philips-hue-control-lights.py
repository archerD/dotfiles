#!/usr/bin/python

from phue import Bridge
import pprint

# hard coded ip address, found by looking at the router settings.
b = Bridge('10.0.0.44')

# only use the first group
group = b.get_group(1)

# print current status
print(f"Any lights on in {group['name']}: {group['state']['any_on']}")
light_status = "["
for light in group['lights']:
    light = b.get_light(int(light))
    status = f"{light['state']['bri']:3}" if light['state']['on'] else "off"
    light_status += f"({light['name']}: {status}), "
print(light_status[:-2]+"]")

# print available scenes and inputs
indexed_scenes = list(enumerate([scene.name for scene in b.scenes]))
print("Available scenes:")
pprint.pp(indexed_scenes)
commands = input("Enter scene index, 'on', 'off', or -brightness value (1-255) (empty cancels): ")

# handle the command(s) given
for command in commands.split():
    if command == "":
        pass
    elif command == 'on':
        b.set_group(1, 'on', True)
    elif command == 'off':
        b.set_group(1, 'on', False)
    elif command.isdigit():
        command = int(command)
        if command >= 0:
            b.run_scene(group['name'], indexed_scenes[command][1])
        else:
            b.set_group(1, 'bri', -command)
    else:
        print("command not recognized")

