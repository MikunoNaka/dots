#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

[ -f $sysresources ] && xrdb -merge $sysresources

[ -f $sysmodmap ] && xmodmap $sysmodmap

[ -f "$userresources" ] && xrdb -merge "$userresources"

[ -f "$usermodmap" ] && xmodmap "$usermodmap"

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

nitrogen --restore &
picom &
dunst &
cbatticon &
#xmodmap .Xmodmap

# launch this from wm or it'll crash
# polybar mybar &

case $1 in 
  xmonad)       exec xmonad ;;
  # openbox)      exec openbox-session & tint2 ;;
  openbox)      exec openbox-session ;;
  bspwm)        exec bspwm ;;
  herbstluftwm) exec herbstluftwm ;;
  spectrwm)     exec spectrwm ;;
  *)            exec xmonad ;;
esac
