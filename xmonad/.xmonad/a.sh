b(){
    answer="$(echo -e "No\nYes" | dmenu -i -p "Kill $selected?" "$@")"
    if [ $answer == 'Yes' ]; then killall xmonad-x86_64-linux ; fi
}
a(){
	dp=$(autorandr | grep detected | sed s/detected// | sed s/current// | sed s/\(\)//g )
    if [ $dp == "move" ]
    then
       echo "xmobar -x 0 $HOME/.config/xmobar/xmobarrc2"
    elif [ $dp == "docked" ]
    then
        echo "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0"
    else
        echo "xmobar -x 0 $HOME/.config/xmobar/xmobarrc2"
    fi
}
while [ ! $# -eq 0 ]
do
	case "$1" in
		--logout | -l)
			b
			exit
			;;
	    --resolution | -r)
			a
			exit
			;;

	esac
	shift
done
