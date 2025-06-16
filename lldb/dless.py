import lldb

def dis_less(debugger, command, result, internal_dict):
    import subprocess
    output = lldb.SBCommandReturnObject()
    debugger.GetCommandInterpreter().HandleCommand("disassemble " + command, output)
    p = subprocess.Popen(['less', '-FRSX'], stdin=subprocess.PIPE)
    p.communicate(input=output.GetOutput().encode())

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand("command script add -h '(dless) disassembly with less pager' -f dless.dis_less dless")
