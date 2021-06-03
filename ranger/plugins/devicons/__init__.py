import os
import stat
import fnmatch

from ranger.api import register_linemode
from ranger.core.linemode import LinemodeBase
from .icons import *

# linemode
# icon - name -

def get_icon(file):
    basename = os.path.basename(file.relative_path)
    mode = file.stat.st_mode
    if stat.S_ISDIR(mode):
        return ''
    icon = file_node_exact_matches.get(basename.lower())
    if icon is not None:
        return icon
    for pattern, ic in file_node_pattern_matches.items():
        if fnmatch.filter([basename],pattern):
            return ic
    return file_node_extensions.get(file.extension, '')

def get_symbol(file):
    # hard link or symbol link
    if file.is_link:
        if not file.exists:
            return '<x~x>'
        if file.stat and stat.S_ISDIR(file.stat.st_mode):
                return '~'
        return '@'
    if file.is_socket:
            return '='
    if file.is_fifo: # pipe
            return '|'
    if file.stat and not file.is_directory:
        mode = file.stat.st_mode
        if mode & stat.S_IXUSR: # executable
            return '*' 
        if stat.S_ISCHR(mode): # character device
            return '-'
        if stat.S_ISBLK(mode): # character device
            return '+'
    return ''


@register_linemode
class DevIconsLinemode(LinemodeBase):
    name = "devicons"
    uses_metadata = False

    def filetitle(self,file,metadata):
        return "{0} {1}{2}".format(get_icon(file),file.relative_path,get_symbol(file))
