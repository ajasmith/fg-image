;; HATCH_THIS_FG
z = RANDOMU(seed,[10,5])-0.5 
i=IMAGE_PLOT_F(z,/SIDE,/BWRDIF,HATCH_THIS=WHERE(z GT 0),$
               RANGE=[-0.5,0.5], $
               COLOUR_TOO_HIGH='Red', $
               COLOUR_TOO_LOW='Blue', $
               TITLE='Hatching positive values',$
               PLOT_POSITION=[0.25,0.12,0.98,0.89], $
               CBAR_POSITION=[0.10,0.12,0.15,0.89], $
               DIMENSION=[450,200])
i -> SAVE, 'hatch_this_fg.png', WIDTH=450
i -> CLOSE
PRINT, 'hatch_this_fg.png created.'



;; IMAGE_PLOT_F
i  = IMAGE_PLOT_F( /TEST )
w  = i[0].WINDOW
w -> SAVE, 'image_plot_f.png', WIDTH=700
w -> CLOSE
PRINT, 'image_plot_f.png created.'


;; FG_COLOURS
f = fg_colours()
f -> SAVE, 'fg_colours.png', width=600
f -> CLOSE
PRINT,     'fg_colours.png created.'



;; MAPPOINTS
x = (RESTORE_TO_STRUCTURE( '/home/jupiter/eodg/smithan/iasi/ash/2014-02-14-Kelut/flag_20140214_1.sav')).flag_ash

mp = MAPPOINTS(  REFORM( x.x[0,*] ), x.lat, x.lon, $
                 RANGE=[-0.5,2], /COLOURBAR, $
                 CBAR_TITLE='Linear AOD approximation for Aso ash', $
                 COLOUR_BACKGROUND='Light Cyan', $
                 COLOUR_LAND='Light green', $
                 COLOUR_TOO_HIGH='Red', $
                 COLOUR_TOO_LOW='Dark Grey', $
                 MAP_PROJECTION='Mollweide' )
mp -> SAVE, 'mappoints.png', WIDTH=600
mp -> CLOSE

END
