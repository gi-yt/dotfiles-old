### EXPORT
export TERM="xterm-256color"                      # getting proper colors
export HISTCONTROL=ignoredups:erasedups           # no duplicate entries
export ALTERNATE_EDITOR=""                        # setting for emacsclient
export EDITOR="emacsclient -t -a ''"              # $EDITOR use Emacs in terminal
export VISUAL="emacsclient -c -a emacs"           # $VISUAL use Emacs in GUI mode

### "bat" as manpager
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# If not running interactively, don't do anything
[[ $- != *i* ]] && return
# PS1='[\u@\h \W]\$ '

 ### PATH
 if [ -d "$HOME/.bin" ] ;
 then PATH="$HOME/.bin:$PATH"
 fi

 if [ -d "$HOME/.local/bin" ] ;
 then PATH="$HOME/.local/bin:$PATH"
 fi

 ### CHANGE TITLE OF TERMINALS
 case ${TERM} in
     xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|alacritty|st|konsole*)
         PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
         ;;
     screen*)
         PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
         ;;
 esac

 ### SHOPT
 shopt -s autocd # change to named directory
 shopt -s cdspell # autocorrects cd misspellings
 shopt -s cmdhist # save multi-line commands in history as single line
 shopt -s dotglob
 shopt -s histappend # do not overwrite history
 shopt -s expand_aliases # expand aliases
 shopt -s checkwinsize # checks term size when bash regains control
 shopt -s extglob
 #ignore upper and lowercase when TAB completion
 bind "set completion-ignore-case on"

 ### ARCHIVE EXTRACTION
 # usage: x <file>
 x ()
 {
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xjf $1   ;;
             *.tar.gz)    tar xzf $1   ;;
             *.bz2)       bunzip2 $1   ;;
             *.rar)       unrar x $1   ;;
             *.gz)        gunzip $1    ;;
             *.tar)       tar xf $1    ;;
             *.tbz2)      tar xjf $1   ;;
             *.tgz)       tar xzf $1   ;;
             *.zip)       unzip $1     ;;
             *.Z)         uncompress $1;;
             *.7z)        7z x $1      ;;
             *.deb)       ar x $1      ;;
             *.tar.xz)    tar xf $1    ;;
             *.tar.zst)   unzstd $1    ;;
             *)           echo "'$1' cannot be extracted via ex()" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
 }

 ### ALIASES ###

 # root privileges
 alias doas="doas --"

 # navigation
 up () {
     local d=""
     local limit="$1"

     # Default to limit of 1
     if [ -z "$limit" ] || [ "$limit" -le 0 ]; then
         limit=1
     fi

     for ((i=1;i<=limit;i++)); do
         d="../$d"
     done

     # perform cd. Show error if cd fails
     if ! cd "$d"; then
         echo "Couldn't go up $limit dirs.";
     fi
 }

 # vim and emacs
 alias em="/usr/bin/emacs -nw"
 alias emacs="emacsclient -c -a 'emacs'"
 # bat
 # alias cat='bat'

 # Changing "ls" to "exa"
 alias ls='exa -hal --color=always --group-directories-first' # my preferred listing
 alias la='ls'
 alias l.='ls | egrep "^\."'

 # pacman and yay
 alias pacsyu='sudo pacman -Syyu'                 # update only standard pkgs
 alias yaysua='yay -Sua --noconfirm'              # update only AUR pkgs (yay)
 alias yaysyu='yay -Syu --noconfirm'              # update standard pkgs and AUR pkgs (yay)
 alias parsua='paru -Sua --noconfirm'             # update only AUR pkgs (paru)
 alias parsyu='paru -Syu --noconfirm'             # update standard pkgs and AUR pkgs (paru)
 alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
 alias cleanup='sudo pacman -Rns (pacman -Qtdq)'  # remove orphaned packages

 # get fastest mirrors
 alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
 alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
 alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
 alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

 # Colorize grep output (good for log files)
 alias grep='grep --color=auto'
 alias egrep='egrep --color=auto'
 alias fgrep='fgrep --color=auto'

 # confirm before overwriting something
 alias cp="cp -i"
 alias mv='mv -i'
 alias rm='rm -i'

 # adding flags
 alias df='df -h'                          # human-readable sizes
 alias free='free -m'                      # show sizes in MB

 ## get top process eating memory
 alias psmem='ps auxf | sort -nr -k 4'
 alias psmem10='ps auxf | sort -nr -k 4 | head -10'

 ## get top process eating cpu ##
 alias pscpu='ps auxf | sort -nr -k 3'
 alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

 # Merge Xresources
 alias merge='xrdb -merge ~/.Xresources'

 # git
 alias addup='git add -u'
 alias addall='git add .'
 alias branch='git branch'
 alias checkout='git checkout'
 alias clone='git clone'
 alias commit='git commit -m'
 alias fetch='git fetch'
 alias pull='git pull origin'
 alias push='git push origin'
 alias stat='git status'  # 'status' is protected name so using 'stat' instead
 alias tag='git tag'
 alias newtag='git tag -a'

 # get error messages from journalctl
 alias jctl="journalctl -p 3 -xb"

 # gpg encryption
 # verify signature for isos
 alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
 # receive the key of a developer
 alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"

 # youtube-dl
 alias yta-aac="youtube-dl --extract-audio --audio-format aac "
 alias yta-best="youtube-dl --extract-audio --audio-format best "
 alias yta-flac="youtube-dl --extract-audio --audio-format flac "
 alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
 alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
 alias yta-opus="youtube-dl --extract-audio --audio-format opus "
 alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
 alias yta-wav="youtube-dl --extract-audio --audio-format wav "
 alias ytv-best="youtube-dl -f bestvideo+bestaudio "

 # switch between shells
 # I do not recommend switching default SHELL from bash.
 alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
 alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"
 alias tofish="sudo chsh $USER -s /bin/fish && echo 'Now log out.'"

 # bare git repo alias for dotfiles
 alias config="/usr/bin/git --git-dir=$HOME/dotfiles --work-tree=$HOME"

 # termbin
 alias tb="nc termbin.com 9999"

 # the terminal rickroll
 alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

 colorscript random

alias vim="nvim"
 ### BASH INSULTER ###
 if [ -f /etc/bash.command-not-found ]; then
     . /etc/bash.command-not-found
 fi
 if [ "$TERM" = "linux" ]; then
	 printf %b '\e[40m' '\e[8]' # set default background to color 0 'dracula-bg'
	 printf %b '\e[37m' '\e[8]' # set default foreground to color 7 'dracula-fg'
	 printf %b '\e]P0282a36'    # redefine 'black'          as 'dracula-bg'
	 printf %b '\e]P86272a4'    # redefine 'bright-black'   as 'dracula-comment'
	 printf %b '\e]P1ff5555'    # redefine 'red'            as 'dracula-red'
	 printf %b '\e]P9ff7777'    # redefine 'bright-red'     as '#ff7777'
	 printf %b '\e]P250fa7b'    # redefine 'green'          as 'dracula-green'
	 printf %b '\e]PA70fa9b'    # redefine 'bright-green'   as '#70fa9b'
	 printf %b '\e]P3f1fa8c'    # redefine 'brown'          as 'dracula-yellow'
	 printf %b '\e]PBffb86c'    # redefine 'bright-brown'   as 'dracula-orange'
	 printf %b '\e]P4bd93f9'    # redefine 'blue'           as 'dracula-purple'
	 printf %b '\e]PCcfa9ff'    # redefine 'bright-blue'    as '#cfa9ff'
	 printf %b '\e]P5ff79c6'    # redefine 'magenta'        as 'dracula-pink'
	 printf %b '\e]PDff88e8'    # redefine 'bright-magenta' as '#ff88e8'
	 printf %b '\e]P68be9fd'    # redefine 'cyan'           as 'dracula-cyan'
	 printf %b '\e]PE97e2ff'    # redefine 'bright-cyan'    as '#97e2ff'
	 printf %b '\e]P7f8f8f2'    # redefine 'white'          as 'dracula-fg'
	 printf %b '\e]PFffffff'    # redefine 'bright-white'   as '#ffffff'
	 clear
 fi
 # VTERM
 vterm_printf(){
     if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
         # Tell tmux to pass the escape sequences through
         printf "\ePtmux;\e\e]%s\007\e\\" "$1"
     elif [ "${TERM%%-*}" = "screen" ]; then
         # GNU screen (screen, screen-256color, screen-256color-bce)
         printf "\eP\e]%s\007\e\\" "$1"
     else
         printf "\e]%s\e\\" "$1"
     fi
 }
 if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
     function clear(){
         vterm_printf "51;Evterm-clear-scrollback";
         tput clear;
     }
 fi
 PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'
 vterm_prompt_end(){
     vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
 }
 PS1=$PS1'\[$(vterm_prompt_end)\]'
 vterm_cmd() {
     local vterm_elisp
     vterm_elisp=""
     while [ $# -gt 0 ]; do
         vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
         shift
     done
     vterm_printf "51;E$vterm_elisp"
 }
 find_file() {
     vterm_cmd find-file "$(realpath "${@:-.}")"
 }

 say() {
     vterm_cmd message "%s" "$*"
 }
 open_file_below() {
     vterm_cmd find-file-below "$(realpath "${@:-.}")"
 }
 if [[ "$INSIDE_EMACS" = 'vterm' ]] \
        && [[ -n ${EMACS_VTERM_PATH} ]] \
        && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
	 source ${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh

     alias vim="find_file"
 fi
 ### SETTING THE STARSHIP PROMPT ###
 eval "$(starship init bash)"
 eval "$(pandoc --bash-completion)"
 # BLE.sh
 source ~/.local/share/blesh/ble.sh
 source ~/.local/share/please-use-xdg.sh
 alias startx="startx ~/.config/X11/xinitrc"

 # Use neovim for vim if present.
 [ -x "$(command -v nvim)" ] && alias vim="nvim" vimdiff="nvim -d"

 # Use $XINITRC variable if file exists.
 [ -f "$XINITRC" ] && alias startx="startx $XINITRC"

 # sudo not required for some system commands
 for command in mount umount sv pacman updatedb su ; do
	 alias $command="sudo $command"
 done; unset command

 # Verbosity and settings that you pretty much just always are going to want.
 alias \
	 cp="cp -iv" \
	 mv="mv -iv" \
	 rm="rm -vI" \
	 bc="bc -ql" \
	 mkd="mkdir -pv" \
	 yt="youtube-dl --add-metadata -i" \
	 yta="yt -x -f bestaudio/best" \
	 ffmpeg="ffmpeg -hide_banner"
 alias diff="diff --color=auto"

 # These common commands are just too long! Abbreviate them.
 alias \
	 ka="killall" \
	 g="git" \
	 sdn="sudo shutdown -h now" \
	 e="$EDITOR" \
	 v="$EDITOR" \
	 z="zathura"
 ## get rid of command not found ##
 alias cd..='cd ..'
 
 ## a quick way to get out of current directory ##
 alias ..='cd ..'
 alias ...='cd ../../../'
 alias ....='cd ../../../../'
 alias .....='cd ../../../../'
 alias .4='cd ../../../../'
 alias .5='cd ../../../../..'
 alias mkdir='mkdir -pv'
 alias path='echo -e ${PATH//:/\\n}'
 alias now='date +"%T"'
 alias nowtime=now
 alias nowdate='date +"%d-%m-%Y"'
 # Stop after sending count ECHO_REQUEST packets #
 alias ping='ping -c 5'
 # do not delete / or prompt if deleting more than 3 files at a time #
 alias rm='rm -I --preserve-root'
 
 # confirmation #
 alias mv='mv -i'
 alias cp='cp -i'
 alias ln='ln -i'
 
 # Parenting changing perms on / #
 alias chown='chown --preserve-root'
 alias chmod='chmod --preserve-root'
 alias chgrp='chgrp --preserve-root'
 ## pass options to free ##
 alias meminfo='free -m -l -t'

 ## get top process eating memory
 alias psmem='ps auxf | sort -nr -k 4'
 alias psmem10='ps auxf | sort -nr -k 4 | head -10'

 ## get top process eating cpu ##
 alias pscpu='ps auxf | sort -nr -k 3'
 alias pscpu10='ps auxf | sort -nr -k 3 | head -10'

 ## Get server cpu info ##
 alias cpuinfo='lscpu'

 ## older system use /proc/cpuinfo ##
 ##alias cpuinfo='less /proc/cpuinfo' ##

 ## get GPU ram on desktop / laptop##
 alias gpumeminfo='grep -i --color memory /var/log/Xorg.0.log'
 alias pubip="dig +short myip.opendns.com @resolver1.opendns.com"
 alias localip="sudo ifconfig | grep -Eo 'inet (addr:)?([0-9]*\\.){3}[0-9]*' | grep -Eo '([0-9]*\\.){3}[0-9]*' | grep -v '127.0.0.1'"
 alias ips="sudo ifconfig -a | grep -o 'inet6\\? \\(addr:\\)\\?\\s\\?\\(\\(\\([0-9]\\+\\.\\)\\{3\\}[0-9]\\+\\)\\|[a-fA-F0-9:]\\+\\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"
 alias pserver="python -m http.server --directory=$1"
 alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort"
 alias gh='history|grep'
 alias count='find . -type f | wc -l'
export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/npmrc
