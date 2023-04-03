#!/bin/bash

merge_figures () {
# define figures
fig1=../img/lme_${1}_fast_fixylim.png
fig2=../img/lme_${1}_normal_fixylim.png
fig3=../img/lme_${1}_slow_fixylim.png

#shave white borders
convert $fig1 -trim +repage tmpfig1.png
convert $fig2 -trim +repage tmpfig2.png
convert $fig3 -trim +repage tmpfig3.png

#add labels
#convert tmpfig1.png -gravity North -pointsize 75 -fill "black" -annotate +300+100 "A" tmpfig1.png
convert -pointsize 60 -fill black -font Liberation-Sans -draw 'text 180 80 "A" ' tmpfig1.png tmpfig1.png
convert -pointsize 60 -fill black -font Liberation-Sans -draw 'text 180 80 "B" ' tmpfig2.png tmpfig2.png
convert -pointsize 60 -fill black -font Liberation-Sans -draw 'text 180 80 "C" ' tmpfig3.png tmpfig3.png

# remove x-axis label of upper and middle figure
convert tmpfig1.png +repage -gravity South -chop 0x35 tmpfig1.png
convert tmpfig2.png +repage -gravity South -chop 0x35 tmpfig2.png

# concatenate images
convert -append tmpfig1.png tmpfig2.png tmpfig3.png ../img/lme_${1}_merged_3speeds.png

}

# run
merge_figures gait_speed_meter_per_second
#merge_figures stride_length_cm
#merge_figures max_sensor_lift_cm

#cleanup
rm tmpfig*.png

