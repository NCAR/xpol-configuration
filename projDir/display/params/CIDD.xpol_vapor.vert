#####################################################################################
#  CIDD.master
#  Configurable Interactive Data Display Master Params
#
#  NCAR/RAP May 2000  -FH.
# 
#  Note:  This is an example file for the Configurable Interactive Data Display (CIDD).
#
#  The order of the various sections of the param file is arbitrary and are delimited by
#   <SECTION_NAME> ...Content... </SECTION_NAME>    tags
#
# Currently Supported sections:
#
#  GRIDS   - Describes each source of data and which color scales to use, etc.
#  WINDS   - Describes each source of wind data.
#  MAPS    - Describes each map overlay source
#  MAIN_PARAMS - Controls most of the features of the Display - Sets domains, menu bars, etc.
#  DRAW_EXPORT - (TDRP syntax) Describes possible products to manually draw.
#  SYMPRODS    - (TDRP syntax) Describes the sources of Symbolic Product Data
#  TERRAIN     - (TDRP syntax) Describes the of Land Use and Terrain Data
#  ROUTE_WINDS  - (TDRP syntax) Describes the Pre-defined Route's Features
#  GUI_CONFIG -   (TDRP syntax)  Describes New GUI Feature Parameters for CIDD
#
#####################################################################################

<GUI_CONFIG>
</GUI_CONFIG>
  
<GRIDS>
# Legend Label: used for all on-screen labels and for movie/html file naming.
#           An Empty grid field can be obtained by using the keyword "None"
## Button Label:  used for all menus.
#
# URL:  is of the form:  mdvp:://host.domain:[port]:dir/dir&Field_name.
#                          (Run the DsServerMgr executable for mdvp service)
#   Spaces can be in field names by substituting a Caret character for the space:
#
#   Field^Name = "Field Name"
#   Field Index can alternatively be specified by prepending a '#' character to the field
#
# Colorscale:  Name of the file containing the value to color mapping for this field.
#
# Units:  A label used on the colorscales, etc.
#
# Cont-lo-hi-interv:  Default/Starting Contour low and high limits and interval.
# Note: Setting the Contour interval to 0.0 tells CIDD to use the colorscale
# for it's colors and values. This feature is used to produce color coded
# line contours.
#
# Rendering Format: The style to render the grid. Current options are:
#  cart:  - Rectangle/polygon fills of gridded data. (note "radial" is also
#           supported for backwards compatibility. CIDD now "autodetects"
#           datasets with non-rectangular grid cells.
#  cont:  - False color filled  contours.
#  lcont:  - Line contours.
#  dcont:  - Dynamic Contours - Automatically switches between cart & cont.
#          - Uses the 'cidd.dynamic_contour_treshold:' parameter
#  Note: Add the word "comp" to the format keyword to turn on composite request by default:
#     example:  cont,comp for a composite color filled contours.
#     example:  cart,comp for a composite filled polygon rendering
#        Add the word "autoscale" to the format keyword to turn on automatic colorscale scaling
#        (In this mode the colorscale will be expanded/shrunk to cover the dynamic range of the
#        data plane in memory, with regular, linear intervals.)
#     example:  cont,comp,autoscale for a composite color filled contours, that is autoscaled.
#
# Display_in_menu: Display this field in the main menu. (1 = Yes 0 = No)
# Bkgnd_Render: Render this field automatically in the background. (1 = Yes 0 = No)
#  Note: This is used to set up "toggleable" field images for comparing two fields,
#  and to select which fields get generated in HTML output mode. 

#----------------------------------------------------------------------------------------------------------
#Legend   Button                                                             Rendering
#Label    Label       URL           Colorscale Units Cont-lo-hi-interv Format Display_in_menu Bkgnd_Render
#----------------------------------------------------------------------------------------------------------
# Front Range
#

# S-BAND MOMENTS

DBZ   DBZ   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&DBZ   dbz.colors   dBZ -5 80 5 radial 1 0
#DBZ_NAA   DBZ_NAA   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&DBZ_NAA   dbz.colors   dBZ -5 80 5 radial 1 0
VEL   VEL   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&VEL   vel.colors   m/s -5 80 5 radial 1 0
WIDTH WIDTH mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&WIDTH  width.colors m/s -5 80 5 radial 1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0

ZDR   ZDR   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&ZDR zdr.colors   dB  -5 80 5 radial 1 0
ZDRM   ZDRM   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&ZDRM zdr.colors   dB  -5 80 5 radial 1 0
PHIDP PHIDP mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&PHIDP phidp.colors deg -5 80 5 radial 1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0

RHOHV RHOHV mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&RHOHV rhohv.colors _   -5 80 5 radial 1 0
NCP NCP mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&NCP ncp.colors _   -5 80 5 radial 1 0
#KDP KDP mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&KDP  kdp.colors   deg/km -5 80 5 radial 1 0     
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0

SNRHC  SNRHC   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&SNRHC  snr.colors   dB     -5 80 5 radial 1 0
SNRVC  SNRVC   mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&SNRVC  snr.colors   dB     -5 80 5 radial 1 0
DBMHC  DBMHC  mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&DBMHC  dbmlow.colors dBm -5 80 5 radial 1 0
DBMVC  DBMVC  mdvp:://$(DATA_HOST):0:/d2/ice/xpol/data/cfradial/moments/xband/vert&DBMVC  dbmlow.colors dBm -5 80 5 radial 1 0

======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0
======== ========  mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation  none.colors   m   -5   80  5  dcont   1 0

</GRIDS>

<MAPS>
#
# MAP_CODE Must be less than 16 characters, Color must be regualr X color name
# Complete map file name used is  map_file_subdir/Map_file_name
# 0 = OFF, > 0 = ON, line of width N
# Detail thresholds are in Km across the image.#
#
#------------------------------------------------------------------------------------------
# MAP_CODE Control_Label   Map_File_Name   On/OFF Detail__min Detail_max Color     
#------------------------------------------------------------------------------------------

#World         World          world_states.map        1  0.0 1000000.0  white

# Colorado

Counties     Counties     us_counties.map   1      0.0      10000.0     blue 
States       States       usa.map           1      0.0      10000.0     white
roads        roads        colo_roads.map    0      0.0      10000.0     white

</MAPS>

<WINDS>
#----------------------------------------------------------------------------
# Note:
# Legend_label: Used for Image Labels and Menus
# URL: should be of the form "protocol://host:port/dir/&" 
#   The U,V and W field names will be appended to complete the URL
#   Use the name "None" to indicate no W field componet.
# U: The  East-West Wind componet Field name 
#   Spaces can be in field names by substituting a Caret character for the space:
#   Example: Field^Name = "Field Name"
#   Field Index can alternatively be specified by prepending a '#' character to the field 
#   (Use this if there are duplicate field names in the data files)
#   Example: First field in file = "#1"
# V: North-South Wind componet Field name 
# W: Up-Down  Wind componet Field name - Use "None" to indicate no W field
#
#  Units: Labels append this (for barbs and reference lines) 
# 
# Style: This column is broken in to several componets for backward compatibility
# 1. A mandantory number must be first.
#     Absolute value of the number is the starting Line width.
#     Values <= 0 Turns it off to begin with
# 2. Second componet is a comma followed by the rendering style:  
#     Wind Rendering Style  Options:
#     "arrow" (centered at data point),
#     "vector",(Vertex at data point )
#     "barb" (N Hemisp),
#     "labeledbarb", (labeled to nearest 10 degrees at the center - N. Hemisp)
#     "tuft", (like a piece of yarn - the least obtrusive - like a headless vector)
#     "tickvector" (Cross ticks at wind_time_scale_interval  minutes)
#     "metbarb" (Calcs latitude - works for both hemispheres and
#     adds a label of the 10's digit off the end of the barb, ala winds aloft charts)
#     "barb_sh" (S Hemisphere),
#     "labeledbarb", (labeled to nearest 10 degrees at the center - N. Hemisp)
#     NOTE: When using arrow,vector,tuft,tickvector,  the data must be in
#     m/sec for the scaling to work correctly. For the others (Barbs),
#     the units are arbitrary, but the flag units on the barbs will reflect
#     the data's native units. 
# Example: "-2,metbarb" -->  Width 2, off to start, using winds aloft chart barbs
# Example: "1"  --> Width 1, On to start, Uses cidd.wind_marker_type: defined in main section
#
# Color: Any Legal X color name - Can be several words, Example: dark green
#
# Legend_label  URL U V W_fieldnames units  Style [Color]
#----------------------------------------------------------------------------
#TREC_mdvp    mdvp:://oberon::dia/mdv/trec/ftg&  #1 #2 None  m/s  2      yellow

</WINDS>

######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
<MAIN_PARAMS>

# Parameter data base for the Configurable Interactive Data Display; CIDD

# Note: One can set these from the command line using the -v flag.
# The value is a bitwise combiation of the following debug
# parameters. Example : -v7 turns on all three -v 3, the first two

# General informational messages - Initialisation messages
cidd.debug_flag:            0

# Data flow related messages  - More verbose
cidd.debug1_flag:           0

# Verbose diagnostics - developer debugging info - 
cidd.debug2_flag:           0

# Uncomment the next parameter to start up cidd in archival mode
#  Starting at the indicated time. Enter HH:MM MM/DD/YY format time string
#  System will startup using field 0, requesting data closest to the
# demo time. The movie frame will be centered on the time after applying
# temporal rounding. 
# cidd.demo_time:  1:00 01/16/08

cidd.use_cosine: 0

# Set times to the nearest value divisible by the number of seconds
cidd.temporal_rounding:    1

#  MOVIE Related resources

cidd.movie_frame_dir: /tmp

#  This determines how many movie frames are in the loop to start with
#  Setting this high will cause the X server to hog memory.
cidd.starting_movie_frames:  12

# minutes per movie frame on startup
# The display will atomatically update every this interval, throwing away its
# oldest movie frame and creating and drawing a new one.
cidd.time_interval:  7.5

# This parameter sets the maximum number of hours in the future a user can select
# from the quick forecast menu. See SHOW_FORECAST_MENU menu bar button
cidd.forecast_interval:  24

# This parameter sets the maximum number of hours in the PAST a user can select
# from the quick forecast menu. See SHOW_PAST_MENU menu bar button
cidd.past_interval:  96

# When a Forecast hour is chosen, magnify the time interval by this factor.
# This adjusts the frame time_interval to be more appropriate for 
# forecast (model) data, which is often output on a more coarse time interval.
# When "Now" is selected the magnification is undone. Default is 1.0
# 
cidd.movie_magnify_factor:  1.0

# Set this paramet to 1 to get CIDD to check and reject for rendering
# Any data that falls outside the movie frame interval + the
# slop/strecch_factor - Used to reject "out of date" data.
cidd.check_data_times:  0

# Sets the allowable time error for data to appear in movie frames
# (times the time_interval) (6 min * 6 = 36 minutes before or after
# the beginning or end of the movie frame times
cidd.stretch_factor:     20.0

# Sets whether Cidd request data closest to the midpoint of a movie
# frame (0) or the end (1). 
# 0= Midpoint requests(Closest_to), 1 = End point requests (First_before).
# Typically, real-time operation should operate as 1 (End point requests)
# Choose mode 0 if you want to enter a time and have data the data
# at that time appear, regardless of how wide the movie frame interval is.
cidd.gather_data_mode:  0

# Number of milliseconds between checks for images needing redrawn
# Set this lower on faster hardware - Recommended 50-250
# This sets the ultimate speed of the movie looping
cidd.redraw_interval:     100

# Number of seconds between data update requests - The display will
# check the Data Mapper for new data every interval in real-time mode.
# Every interval CIDD will check for updates to each grid and product. If
# no datamap_host is set, the data is assumed to be old and will be reloaded
# every interval. Set this wisely. Values of 10-900 are typical. Default=120
# Setting this below 3 seconds is not advised.  interacts with the Data
# Mapper Host parameter: cidd.datamap_host:

cidd.update_interval:      30

# Data Mapper Host
#cidd.datamap_host:  venus

#  Number of seconds to wait for data to come in before giving up
#  On very slow networks this may need to go as high as 180 seconds!
cidd.data_timeout_secs: 40

# Length of time to wait before killing simple external commands
# Used When running single image convert scripts and driving
# Browsers to web pages.
cidd.simple_command_timeout_secs: 30

# Length of time to wait before killing complex external commands
# Used when spawining animation building script
cidd.complex_command_timeout_secs: 180

# Set to 1 to start up cidd in movie-on mode, 0 = off
cidd.movie_on: 0

# Set the delay at the end of the movie loop in msec 
cidd.movie_delay: 3000

# Set the speed of the movie loop. - (msec per frame)
cidd.movie_speed_msec: 75

# If set to 1 - forces CIDD to reload all data every time the movie frames 
# rotate one old frame out and generate a new frame.
cidd.reset_frames:   0

# Gather model run times within these number of hours from the end
# of the movie loop. More hours means more Model runs wll appear in the menu.
cidd.model_run_list_hours:  24   

# After this period of inactivity, the display
# will reset itself to a known starting point
# Set this <= 0 to essentially turn off.
cidd.idle_reset_seconds: 0

# HTML Mode - Set this to 1 to put CIDD into the automatic HTML output mode.
# Note: cidd.image_dir MUST be set.
cidd.html_mode: 0

# Image Generation related resources
# Set this to a value to get cidd to output imagery as XWD in this directory
cidd.image_dir: /tmp

# Set to 1 if one wants CIDD to run once through it's HTML output
# plot list and then exit - Currently all fields at each zoom level
cidd.run_once_and_exit: 0

# The HTML MODE Output file name is built from parts:
# 'cidd' + fieldname + zoom_level + [height] +
#  frame_time + [data_generation_time] + [data_valid_time] + .'xwd'

# When set to 1 forces the output filename to contain height
# information in it's file name.  See the comment above on the order
cidd.add_height_to_filename: 1

# When set to 1 forces the output filename to contain the data's generataion
# time in it's file name.  See the comment above on the order
cidd.add_gen_time_to_filename: 0

# When set to 1 forces the output filename to contain  the data's valid
# time in it's file name.  See the comment above on the order
cidd.add_valid_time_to_filename: 0

# Set the format for the time fields in the names of output xwd images.
# See strftime(3) for options.  Default is: %H%M
cidd.movieframe_time_format:  %H%M

# This script will be exec'd if set whenever an html_mode image is output.
# The full path name of the xwd image will be sent as the script argument
cidd.image_convert_script: convert_image.csh

# This script will be exec'd if set whenever a print command is issued.
# The full path name of the xwd image will be sent as the script argument
cidd.print_script: convert_image_print.csh

# This script will be exec'd if set whenever the user selects the Output Loop
# Button, after the last image has output. The arguments will be a list
# of each output file in the series. CIDD will prepends the cidd.image_dir to each
# files name which takes the form: cidd_field_name_zoom_name.xwd 
cidd.series_convert_script: make_anim.csh

# Set to 1 to ask that data be compressed during the MDV Protocol
# data requests. Default is = 0
cidd.request_compressed_data: 1
 

#                  DOMAIN RELATED RESOURCES 
#
# Sets the projection for cidd's plan view - Current valid choices are:
# CARTESIAN, LAT_LON, LAMBERT,  STEREOGRAPHIC
cidd.projection_type:    CARTESIAN

# These must be set if using a Lambert conformal projection - Ignored otherwise
cidd.lambert_lat1: 20.0
cidd.lambert_lat2: 60.0 

# These must be set when using a STEREOGRAPHIC (Polar stereo or Oblique stereo)
cidd.tangent_lat: 20.0
cidd.tangent_lon: -104.0

# Set this to the amount grids are rotated clockwise to true north.
# Overlays will rotate counterclockwise. - In Degrees
# Only valid for CARTESIAN projection
cidd.north_angle:        0.0

# Set the scale factor and label for Range rings and Distance scales.
#cidd.units_per_km:      1.0

# Set the following to Deg for LAT_LON projections - Default is "km"
#cidd.scale_units_label:  nm

# When set to 1, always gets full domain, zooms don't force a data request
cidd.always_get_full_domain:  0

# Set the aspect ratio of the domain for Cidd. Cidd will preserve this aspect
# ration for all zooms. Ratio is Width/Height.
# If set <= 0.0, CIDD will compute the aspect ratio based on the extent of the
# first domain.
cidd.aspect_ratio:   1.0
#cidd.aspect_ratio:   1.3

# Switch to enable the status window. 0 = off
cidd.enable_status_window: 0
 
# Switch to enable the single middle button click save image feature
# Warning - WiLL allow users shell access, or to write to other dirs.
cidd.enable_save_image_panel: 1
 
# On startup and each time the reset button is pressed, the display
# will indicate a click at this point, as if the user had clicked..
cidd.reset_click_longitude:  0
cidd.reset_click_latitude:   0
#
# The Outer most domain: Overlays are clipped to this domain and 
# pan/moves cannot exceed these edge limits.
cidd.domain_limit_min_x:        -1000
cidd.domain_limit_max_x:         1000
cidd.domain_limit_min_y:        -1000
cidd.domain_limit_max_y:         1000
 
# The Lat, Long of the center of the display. Internal range rings
# are relative to this point.
cidd.origin_longitude:       -105.24
cidd.origin_latitude:         40.03850
#
#
# Select the data fields/page to start on.
cidd.planview_start_page:   1
cidd.xsect_start_page:      1
#

#
# The Outer most domain:
cidd.num_zoom_levels:       4
cidd.start_zoom_level:      1

# The number of Cached images for zooms.
cidd.num_cache_zooms:       4
#
# Note, the Domain labels are used for automated output imagery.
# It's suggested that names without spaces be used when running
# in.
#
cidd.level1_label:                  75km
cidd.level1_min_xkm:                -75.0
cidd.level1_min_ykm:                -75.0
cidd.level1_max_xkm:                75.0
cidd.level1_max_ykm:                75.0
#
#
cidd.level2_label:                  40km
cidd.level2_min_xkm:                -40.0
cidd.level2_min_ykm:                -40.0
cidd.level2_max_xkm:                40.0
cidd.level2_max_ykm:                40.0
#
#
cidd.level3_label:                  150km
cidd.level3_min_xkm:                -150.0
cidd.level3_min_ykm:                -150.0
cidd.level3_max_xkm:                150.0
cidd.level3_max_ykm:                150.0
#
cidd.level4_label:                  300km
cidd.level4_min_xkm:                -300.0
cidd.level4_min_ykm:                -300.0
cidd.level4_max_xkm:                300.0
cidd.level4_max_ykm:                300.0
#

# Vertical Range and Resolution - Establishes a vertical coordinate system
# for the cross sectional imagery.
# All data must map into this range for it to be visible. 
# min_ht is value nearest the ground - Plotted at the bottom.
# max_ht is the value furtherest from the ground - Plotted at the top.
#
# Units are ignored and no interconversions or re-mappings are performed.
# Returned cross sections are are often km, but could be sigma, pressure or degrees elevation. 
# Examples:
# Sigma : min_ht = 1.0, max_ht = 0.0
# Pressure : min_ht = 1100.0, max_ht = 100.0
# Radial Radar:  min_ht = 0.0 max_ht = 25.0
#  
cidd.min_ht:               0.0
cidd.max_ht:               20.0
cidd.start_ht:             0.5

# Radial data Limits
cidd.min_rkm:                0.0
cidd.max_rkm:                400.0
cidd.min_adeg:               0.0
cidd.max_adeg:               360.0

# List of comma delimited local directories or HTTP URLs to search
# Searchs . then each directory/URL in order 
cidd.color_file_subdir:   $(HOME)/xpolProjDir/display/color_scales
cidd.map_file_subdir:     $(HOME)/xpolProjDir/display/maps
#cidd.map_file_subdir:     http://spol/cidd/display/maps

#
# Station locator features
#
# Maximum allowed  Distance in km between request point and station
cidd.locator_margin_km:   50
# Full Path File name or http URL to a station data file.
# The format of each line is:  ID, LAT, LON, ALT(m), TYPE
#cidd.station_loc_url: http://www.rap.ucar.edu/maps/Taiwan_stations.dat 

#
# FMQ Type URL for Receiving Remote commands
#cidd.remote_ui_url: /tmp/remoteUI

# Tunnel for all DsServer (mdvp,spdbp,etc) Protocols
# USE THIS TO CROSS A FIREWALL ON THE SERVER END OF THE CHAIN 
#Set this to the special http tunnel that resides in the same local
#network as the  data server hosts. This allows "tunneling" the MDV and
#Spdb data protocols through a host, running apache:httpd that is visible
#from the internet or intranet. Note: a special  "Apache::Mod_perl"
#extension (The tunnel - it's similar to a cgi mechanism) must be
#installed on the http server for this to work.
#
#cidd.http_tunnel_url: http://www.rap.ucar.edu/DsServerTunnel
#
# USE THIS TO CROSS A FIREWALL AT THE Client/CIDD End of the Chain
# Set this to the url of a http forwarding proxy to pass 
# http requests through the proxy. 
# This is usually the same as the HTTP proxy URL in your browser.
# Note. If you need the data to flow through the proxy
# the http_tunnel_url must be enabled
#
# This is NCAR's internal HTTP proxy
#cidd.http_proxy_url: http://webcache.ucar.edu:3128/  
#
# Another view of the possible Service Topologies.
# 
# All Local - No Firewalls - Standard Setup 
#--------- Local Lan --------------
# CIDD<--- MDVP --->DsMdvServer
# 
# Data Services Behind a firewall
#----- Internet ----|Firewall|------ Local Lan ----------------
# CIDD<---HTTP--->DsServer Tunnel<---MDVP--->DsMdvServer
#  
## Both Client and Data Services behind Firewalls
#---Local Lan ---|Firewall|--Internet----|Firewall|--------Local Lan ----------
# CIDD<---HTTP--->Http Proxy<---HTTP--->DsServer Tunnel<---MDVP--->DsMdvServer
#
#

#  Plan View and Cross Section COLORS
#
# Legends, scales, etc use this color
cidd.foreground_color: white

# Background of Images use this color
cidd.background_color: black

# Background of margins will be this color
cidd.margin_color: black

# Render values outside the data range of the color scale in this color
# 'transparent' is the default
cidd.out_of_range_color: gray20

# Color of the cross section reference line 
cidd.route_path_color: yellow

# Text in the time axis margins are this color
cidd.time_axis_color: pink

# A solid rectangle  showing the movie frame extent is drawn in this color
cidd.time_frame_color: gray50

# The text in the height selector margin is rendered in this color
cidd.height_axis_color: pink

# A solid rectangle showing the height extent of the data plane is drawn in this color
cidd.height_indicator_color: gray50

# Range rings and azmiuth lines
cidd.range_ring_color: gray

# 
# Set the follwing colors to 'transparent' to avoid having these
# plotted. (This is the default) Otherwise, these data will be
# plotted in the indicated color
cidd.missing_data_color: grey20

cidd.bad_data_color: grey20
#
# Time Control (Movie) Popup Colors
cidd.epoch_indicator_color: orange
cidd.now_time_color: red
cidd.time_tick_color1: cyan
cidd.time_tick_color2: yellow
cidd.time_tick_color3: magenta
cidd.time_tick_color4: green
cidd.time_tick_color5: red
cidd.time_tick_color6: blue

#Toggle for displaying the height Selector in Right Margin
# Note Right Margin shoud be at least 50 pixels wide
cidd.show_height_sel:       1

# Display data access and rendering messages - This is probably not
# necessary in situations where all data is small and local.
cidd.show_data_messages: 1

# Sets the relative intensity of the image, relative to the overlays on startup
# Set between 0.2 and 1.0.
cidd.image_inten:            0.8
# The number of selectable intensity levels in the slider control
cidd.inten_levels:            32

# Sets the threshold in drawable grid points where image rendering 
# is done by a filling method on the client, rather than by rectangles
# on the Xserver.
# Set this high if the X server is fast or the comm link to the server is slow.
# Set this low if the X server is slow. Typical values range from 10,000 to
# 100,000  Used for 'cart' style rendering.

cidd.image_fill_threshold:    80000

# This sets the threshold in grid cells when the the display will
# render a grid as contours or as filled rectangles. Grids contining
# less than the threshold cells will be rendered as filled contours.
# used for 'dcont' style rendering
cidd.dynamic_contour_threshold:    160000

# Sets the format for output of Lat, Lon's. 0= Decimal Degrees, 1 = deg min' sec"
cidd.latlon_mode:        0

# Set the format for the time strings. See 'man strftime' for format options.
# Can add additional text as part of the Time string.
# Default is: "%m/%d/%Y %H:%M:%S"
cidd.label_time_format:  %m/%d/%Y %H:%M:%S

# Plot a Legend for each Grid overlay; 0=Off, 1=On
cidd.layer_legends_on: 1  

# Plot a Legend for each Contour overlay; 0=Off, 1=On
cidd.cont_legends_on: 1

# Plot a Legend for each Wind overlay; 0=Off, 1=On
cidd.wind_legends_on: 1

# Plot a Legend for each Symbolic Product overlay; 0=Off, 1=On
cidd.symprod_legends_on: 1
    
# Switch to turn off all data labels on the display
cidd.display_labels: 1  
#
# Switch to turn on/off Height and Route reference lines
cidd.display_ref_lines: 1
# 

# TOP Margin styles:
# 1. Default. Places a distance scale in the top Margin.
# 2. Place a Frame number and Time - Used often for HTML output.
# 3. None: -  Used often to place the Data label
#
cidd.top_margin_render_style: 1

# Bottom Margin Styles
# 1 - Distance Scale
# 2 - Time Scale
cidd.bot_margin_render_style: 2

#
# Explititly set the Label start coords, and delta Y.
# If left unset, ledgends start relative to the top and left margins.
#
cidd.horiz_legends_start_x:  30
cidd.horiz_legends_start_y:  40
cidd.horiz_legends_delta_y:  30

#
# WINDOW SIZE PARAMS
#
cidd.horiz_default_height:    900
cidd.horiz_min_height:        400
cidd.horiz_min_width:         400
cidd.horiz_default_x_pos:     0
cidd.horiz_default_y_pos:     0

cidd.horiz_top_margin:        30
cidd.horiz_bot_margin:        20
cidd.horiz_left_margin:       20
cidd.horiz_right_margin:      60

cidd.vert_min_height:        120
cidd.vert_min_width:         120
cidd.vert_default_height:    440
cidd.vert_default_width:     600

cidd.vert_top_margin:        40
cidd.vert_bot_margin:        20
cidd.vert_left_margin:       20
cidd.vert_right_margin:      30


# Set range_ring_spacing to a negative value to dynamically determing spacing.
cidd.range_ring_spacing:     25.0
# x_space, y_space in pixels determine allowable space for range ring labels
cidd.range_ring_x_space:    40
cidd.range_ring_y_space:    13
cidd.azmith_interval:       30.0
cidd.azmith_radius:         200.0

# Toggles to turn on/off  Extras features on startup
cidd.range_rings:             1
cidd.azmith_lines:            1

#  Render Range rings at the origin of the data, instead of the origin of the display.
cidd.range_ring_follows_data: 1

#  Shift the Domain if the data shifts.
cidd.domain_follows_data: 1

# Starting state of the All Winds ON/OFF feature
cidd.all_winds_on: 0

# Wind vector parameters 
cidd.barb_shaft_len:         30
cidd.wind_head_size:         5
cidd.ideal_x_vectors:        20
cidd.ideal_y_vectors:        20
cidd.wind_head_angle:        30.0
#

#  Minutes between steps on the wind scaling slider. Wind vector 
#  size starts at 3 times this interval.
#  Starting/Default length = 3 * wind_time_scale_interval *  wind velocity
cidd.wind_time_scale_interval: 10.0

# Wind Rendering Style  Options:
# "arrow" (centered at data point),
# "vector",(Vertex at data point )
# "barb" (N Hemisp),
# "labeledbarb", (labeled to nearest 10 degrees at the center - N. Hemisp)
# "tuft", (like a piece of yarn - the least obtrusive - like a headless vector)
# "tickvector" (Cross ticks at wind_time_scale_interval  minutes)
# "metbarb" (Calcs latitude - works for both hemispheres and
# adds a label of the 10's digit off the end of the barb, ala winds aloft charts)
# "barb_sh" (S Hemisphere),
# "labeledbarb", (labeled to nearest 10 degrees at the center - N. Hemisp)       
# NOTE: When using arrow,vector,tuft,tickvector,  the data must be in
# m/sec for the scaling to work correctly. For the others (Barbs),
# the units are arbitrary, but the flag units on the barbs will reflect
# the data's native units.
cidd.wind_marker_type:      vector
cidd.wind_w_scale_factor:    10.0
#
# Set this value to convert from the your reference speed units 
# to m/sec.  ie wind_units_scale_factor =  m/sec / chosen units
# default = 1.0
cidd.wind_units_scale_factor:     0.2778

# Sets the length of the wind vector reference speed legend.
cidd.wind_reference_speed: 30.0

# Select the label to be printed next to the reference bar
# Default = m/sec
cidd.wind_units_label:  km/hr

# Uncomment the following lines to turn on default contour fields
# Uses the contour levels and the Field Label string in data.info
# The parameters are Exact_field_label and color and optional "off"
# Keyword. If "off" param is used, the field will be defined, but will
# not be acitvated on startup.
#cidd.contour1_field: FzLevel red off
#cidd.contour2_field: SURF_T white
#cidd.contour3_field: SURF_T white
#cidd.contour4_field: SURF_T white
#cidd.contour5_field: SURF_T white
# 
# Set to 1 to have numeric labels on line contours, 0 = off
cidd.label_contours:   0           

#  Default line width of contours
cidd.contour_line_width: 1

# Apply smoothing to data grids before contouring
# 0 = Off, 1 = single pass, 2 = double pass
cidd.smooth_contours: 0 

# Add 1 part in 250 Noise to help smooth contours
cidd.add_noise: 0 

# Use Alternate (Mdvx) Contouring Routines -
# Slower - Nicer labeling - Less smooth - 
# Warning - Does not work correctly when data and dislay origins are different
# (Works fine in lat/lon projections)
cidd.use_alt_contours: 0 

#
# Contours at this value will have a wider line
# 
# 
cidd.special_contour_value: 0.25

# Sometimes, for CONTOURING it is useful to make
# the assumption that the bad and/or missing values
# should be considered at the minimum of the dynamic range of the
# data. IE. If you want bad an/or missing contoured
# set these to 1, otherwise, set them to 0 (default)
#
cidd.map_bad_to_min_value: 0
cidd.map_missing_to_min_value: 0
#
# Uncomment the following lines to set up fields on startup that will
# appear as overlaid fields. Layers are on by default unless the
# optional "off" string appears last. 
#cidd.layer5_field:  KCYS-DBZ 
#cidd.layer4_field:  KPUX-DBZ
#cidd.layer3_field:  KGJX-DBZ
#cidd.layer2_field:  KGLD-DBZ off
#cidd.layer1_field:  KLNX-DBZ off

# Rendering order for layers:
#  0: render main field first then layers
#  1: render main field on top of layers.
cidd.draw_main_on_top: 0


# Place Cidd fonts in asending order of size. Cidd will choose the
# highest font number that will fit in a given screen space.
# Pick a nice spectrum of font sizes, with all of them readable.
# CIDD currently accepts up to 16 different fonts
cidd.num_fonts: 4
#
cidd.font1: 6x9
cidd.font2: -*-courier-medium-r-normal-*-10-*-*-*-*-*-*-*
cidd.font3: -*-courier-medium-r-normal-*-12-*-*-*-*-*-*-*
cidd.font4: -*-courier-bold-r-normal-*-12-*-*-*-*-*-*-*
cidd.font5: -*-courier-bold-r-normal-*-14-*-*-*-*-*-*-*
cidd.font6: -*-courier-bold-r-normal-*-15-*-*-*-*-*-*-*
cidd.font7: -*-courier-bold-r-normal-*-16-*-*-*-*-*-*-*
cidd.font8: -*-courier-bold-r-normal-*-18-*-*-*-*-*-*-*
cidd.font9: -*-courier-bold-r-normal-*-24-*-*-*-*-*-*-*
cidd.font10: -*-courier-bold-r-normal-*-30-*-*-*-*-*-*-*

# font_display_mode = 1 - Clears background behind label text., 0 - No
cidd.font_display_mode: 0
# 
# 
# Manual Image save resources    
# Note cidd appends the image name to the command string before executing
# This allows the script to pull the name off the command line,
# Otherwise it is often useful to place a "rm" as the last command to remove
# the image after printing, conversion, etc
#
# Saved images go here
cidd.horiz_image_dir:   /tmp
# The file name will be until reset by the user - Appends '.xwd' automatically
cidd.horiz_image_fname:   CH
# Then the post processing command is run, with the xwd file as the first argument
cidd.horiz_image_command:  convert_image.csh
#
cidd.vert_image_dir:    /tmp
cidd.vert_image_fname:    CV
cidd.vert_image_command:  convert_image.csh


#Toggle for displaying an analog clock in the upper right hand corner of the image
cidd.show_clock:       0
# Set to 0 to draw the analogue clock using GMT, 1 = local time.
cidd.draw_clock_local: 0

cidd.one_click_rhi: 1
cidd.click_posn_rel_to_origin: 1

#####################################################################################
# GUI RELATED STUFF
#
# Set this parameter to force the field menu popup to this many columns
# If set <= 0, then cidd sets this dynamically, allowing at most 30 in each
# column.
cidd.num_field_menu_cols: 4

# This parameter sets WSDDM specific behavior On -
# - Go to end of loop when movie stops.
# - No on-image no-data message when winds are missing
cidd.wsddm_mode:            0

# 
# MENU_BAR Configuration
# The Menu bar is configurable. One can configure which buttons/functions are
availible to the user. In many operational setting many of the fuctions should
not be availible.  For research/ post analysis most of them should be enabled. 
#####################################################################################
# Defines for the Menu Bar at the top of the main window. Functions are:
#
#    --- One Shot Functions ---
# RELOAD - (Force all data to be reloaded)
# RESET - (Set display to starting zoom, field, products on/off state, etc)
# SET_TO_NOW - (Sets the display to current time - canceling forecast mode)
# SET_DRAW_MODE - (Enable the Human entered feature drawing mode)
# PRINT_BUTTON - (Save and Print the plan view image) 
# HELP_BUTTON - (Send Netscape to a useful help Page)
# EXIT_BUTTON - Kills the Display
# CLONE_CIDD - Starts another instance of the same display 
# SNAP_IMAGE - Starts A Snapshot viewer - Displaying the current image
#
#    --- Toggle things on and off ----
# LOOP_ONOFF - (Turn the movie loop on/off)
# WINDS_ONOFF - (Turn all winds on/off)
# SYMPRODS_ONOFF - (Turn all Symbolic Products on/off)
# REPORT_MODE_ONOFF - (Turn on/off COntinious REport Mode )
#
#    --- Display Panels ---
# SHOW_DPD_PANEL - (Show/Hide Page Definition Panel which configures individual pages)
# SHOW_XSECT_PANEL - (Show/Hide the Cross Section Display View Panel)
# SHOW_GRID_PANEL - (Show/Hide the Data Source Configuration Panel)
# SHOW_STATUS_PANEL - (Show/Hide a Status message window)
# SHOW_TIME_PANEL - (Show/Hide the Movie loop control Panel) 
# CLOSE_POPUPS  - (Close all Popup panels)
#
#    --- Display Additional Menus ---
# SHOW_DPD_MENU - (Show/Hide the field/page description menu)
# SHOW_VIEW_MENU - (Show/Hide the zoom level menu)
# SHOW_MAP_MENU - (Show/Hide the map overlay menu)
# SHOW_PRODUCT_MENU - (Show/Hide the symbolic product menu)
# SHOW_FORECAST_MENU -(Show/Hide the forecast mode menu)
# SHOW_PAST_MENU    - (Show/Hide the past mode menu)
# SHOW_BOOKMARK_MEN - (Show/Hide the helpful bookmark menu)
# SHOW_GENTIME_MENU - (Show/Hide the Model Generation Time menu)
# SHOW_CMD_MENU  -    (Show/Hide the Command menu)
# 
#
#####################################################################################
cidd.num_menu_bar_cells: 16
#
cidd.menu_bar_label1: Fields
cidd.menu_bar_funct1: SHOW_DPD_MENU
# 
cidd.menu_bar_label2: View
cidd.menu_bar_funct2: SHOW_VIEW_MENU
#
cidd.menu_bar_label3: Maps
cidd.menu_bar_funct3: SHOW_MAP_MENU
#
cidd.menu_bar_label4: Movie
cidd.menu_bar_funct4: SHOW_TIME_PANEL
#
cidd.menu_bar_label5: Loop
cidd.menu_bar_funct5: LOOP_ONOFF
#  
cidd.menu_bar_label6: Now
cidd.menu_bar_funct6: SET_TO_NOW
#
cidd.menu_bar_label7: Overlays
cidd.menu_bar_funct7: SHOW_DPD_PANEL
#
cidd.menu_bar_label8: Products
cidd.menu_bar_funct8: SHOW_PRODUCT_MENU
#
cidd.menu_bar_label9: Vsection
cidd.menu_bar_funct9: SHOW_XSECT_PANEL
#
cidd.menu_bar_label10: Reset
cidd.menu_bar_funct10: RESET
#
cidd.menu_bar_label11: Reload
cidd.menu_bar_funct11: RELOAD
#
cidd.menu_bar_label12: Value
cidd.menu_bar_funct12: REPORT_MODE_ONOFF
#
cidd.menu_bar_label13: Config
cidd.menu_bar_funct13: SHOW_GRID_PANEL
#
#cidd.menu_bar_label14: CLONE
#cidd.menu_bar_funct14: CLONE_CIDD
#
cidd.menu_bar_label14: Status
cidd.menu_bar_funct14: SHOW_STATUS_PANEL
#
cidd.menu_bar_label15: Exit
cidd.menu_bar_funct15: EXIT_BUTTON
#
cidd.menu_bar_label16: Back
cidd.menu_bar_funct16: ZOOM_BACK
#
#cidd.menu_bar_label10: Symprods on/off
#cidd.menu_bar_funct10: SYMPRODS_ONOFF
#
#cidd.menu_bar_label17: Help
#cidd.menu_bar_funct17: HELP_BUTTON
#
#cidd.menu_bar_label7: URL
#cidd.menu_bar_funct7: SHOW_BOOKMARK_MENU
#
#cidd.menu_bar_label8: Model Run
#cidd.menu_bar_funct8: SHOW_GENTIME_MENU
#
#
#cidd.menu_bar_label12: Print
#cidd.menu_bar_funct12: PRINT_BUTTON
#
#cidd.menu_bar_label14: Fcast
#cidd.menu_bar_funct14: SHOW_FORECAST_MENU
#
#cidd.menu_bar_label15: Past
#cidd.menu_bar_funct15: SHOW_PAST_MENU
#
#cidd.menu_bar_label17: Close Popups
#cidd.menu_bar_funct17: CLOSE_POPUPS
#
#cidd.menu_bar_label19: Winds
#cidd.menu_bar_funct19: WINDS_ONOFF
#
#cidd.menu_bar_label24: Draw Mode
#cidd.menu_bar_funct24: SET_DRAW_MODE
#
#cidd.menu_bar_label27: Commands
#cidd.menu_bar_funct27: SHOW_CMD_MENU
#
#cidd.menu_bar_label28: Snapshot
#cidd.menu_bar_funct28: SNAP_IMAGE
#
#cidd.menu_bar_label29: Base Map
#cidd.menu_bar_funct29: LANDUSE_ONOFF
#


##########################################

cidd.horiz_frame_label: SPOL MARSHALL

# Set the message to display when no data is availible
cidd.no_data_message:     NO DATA FOUND (in this area at the selected time)

# System NO-Data  Status/Info File
# Cidd will check for updates to this file and will Replace the no_data_message
# With the text from this file
cidd.status_info_file:

# HELP TEXT LINKS                       
# These take the form:
# cidd.widget_name_help:       file_name:file_index_key_string
# The HELPPATH envoronment variable must be set or the help file
# must be located in /ussr/lib/help.
# Note the file name gets expanded to file_name.info
#
cidd.canvas_help:              cidd_help:canvas
cidd.frame_message_help:       cidd_help:frame_message

cidd.help_command:   netscape_starter -remote 'openURL(http://www.rap.ucar.edu/CIDD/mds_user_manual/)'

#
# This command is run whenever a BOOKMARK_MENU button is pressed.
# The characters; %U are replaced with the url text from the
# parameters; cidd.bookmarkN below.
cidd.bookmark_command:  netscape_starter -remote 'openURL(%U)'

# Number of bookmarks in the Menu
cidd.num_bookmarks: 4

# BOOKMARKS FOR Quick Reference
# Create N Bookmark and Label parameters
# cidd.bookmarkN: - The exact URL text for use in the cidd.bookmark_command.
# cidd.bookmark_labelN: The Label that appears in the menu
#
cidd.bookmark1: http://mars/aoaws/doc/roc_mds.html
cidd.bookmark_label1: ROC
cidd.bookmark2: http://www.rap.ucar.edu/weather/
cidd.bookmark_label2: RAP Weather
cidd.bookmark3: http://adds.awc-kc.noaa.gov/
cidd.bookmark_label3: ADDS
cidd.bookmark4: http://cessna/
cidd.bookmark_label4: ADDS-Cessna


# Resources for compatibility with TITAN-TIME HISTORY Program

cidd.track_server_instance:         Operations
cidd.track_past_plot_period:        0
cidd.track_future_plot_period:      0
cidd.track_n_forecast_steps:        5
cidd.track_forecast_interval:       6
cidd.track_data_time_margin:        750
cidd.track_past_color:              yellow
cidd.track_current_color:           cyan
cidd.track_future_color:            blue
cidd.track_forecast_color:          red

#  KEYS for SHARED MEMORY SEGMENTS

# Key for coordinate shared memory
cidd.coord_key:               61500

# Key for TITAN control shared memory
cidd.titan_key               61501


#  Starting Size size of fonts for symbolic products - From above list
cidd.product_font_size: 4

#Detail thresholds and font adjustment parameters. First implicit threshold is 0.0
# If the current distance across a domain is between the thresholds, then an adjustment is made to the
# default/selected font size. This is used to change the size of the fonts
# automatically as the users zooms in and out. Threshold values are in km across the
# viewing domain
cidd.product_detail_threshold1: 50.0
cidd.product_detail_adjustment1: +3
#
cidd.product_detail_threshold2: 200.0
cidd.product_detail_adjustment2: +2
#
cidd.product_detail_threshold3: 400.0
cidd.product_detail_adjustment3: +1

# Product Scaling Constant. - Set this constant to determine
# at what zoom icons and will have a scale factor of 1.0
# Default = 300. Function = log10(Constant /km_across_screen) + 1.0;
cidd.scale_constant: 300.0 

# prevent CIDD from closing popup windows when changing desktops

cidd.close_popups: 0

# RHI support

# CIDD uses the number pad keys 7,9,4,6,1,3 to rotate the RHI
# by +/- these amounts (degrees)
# 7-9 is coarse 4-6 is medium and 1-3 is fine.
cidd.rotate_coarse_adjust: 6
cidd.rotate_medium_adjust: 2
cidd.rotate_fine_adjust:  1

</MAIN_PARAMS>

<#DRAW_EXPORT>
///////////// dexport_info ///////////////////////////////
// Draw/Export Parameters
//
// Each Entry needs:
// ID_Label FMQ_URL Valid_Minutes Default_ID_no
//    ID_LABEL - This is used to designate Product types
//    FMQ_URL: example: fmqp:://host::dir/file
//    Valid_Minutes: How many minutes the product is valid by default
//    Default_ID_no: Default Data Type (Enumerated ID)
//

dexport_info = {
    {"Boundry",         "/tmp/cidd.fmq",                           60, 0}
};
<#/DRAW_EXPORT>


<SYMPRODS>
///////////// debug ///////////////////////////////////
//
// Debug option.
// If set, debug messages will be printed appropriately.
//
// Type: enum
// Options:
//   DEBUG_OFF, DEBUG_NORM, DEBUG_VERBOSE
//
//

debug = DEBUG_OFF;

///////////// short_requests //////////////////////////
//
// Gather Data Frame by Frame, Otherwise by loop span.
// Gathers Products valid for the current frame only.
// Type: boolean
//
   
short_requests = TRUE;

///////////// prod_info ///////////////////////////////
//
// Product Source and Rendering Information Array
// The fields in the structure are as follows:
//   menu_label: Label to be used for the product in menu.
//
//   url: for retrieving the data.
//
//   data_type: data type used when querying the SPDB data server
//     (set to 0 to retrieve all data regardless of type).
//
//   url: URL for the data. -
//     example: spdbp:transform_executable:[param_file]//host.dom:[port]:dir1/dir2
//
//   render_type: Select how temporal clipping is done:
//      RENDER_ALL: Do no temporal clipping.
//      RENDER_ALL_VALID: Render all Valid in frame
//      RENDER_VALID_IN_LAST_FRAME: Render all Valid in the last frame
//      RENDER_LATEST_IN_FRAME: Render the latest product in the frame.
//      RENDER_LATEST_IN_LOOP:  Render the latest product valid in the movie loop
//      RENDER_FIRST_BEFORE_DATA_TIME: Render the latest product before the data time in each frame.
//      RENDER_FIRST_AFTER_DATA_TIME:Render the earliest product after the data time in each frame.
//      RENDER_ALL_BEFORE_DATA_TIME: Render all valid products before the data time in each frame.
//      RENDER_ALL_AFTER_DATA_TIME:  Render all valid products after the data time in each frame.
//        Note: all comparisions are less than or equal or greater than or equal  
//
//   on_by_default: True or False
//
//   minutes_allow_before: Stretch the frame time this many minutes - prior
//   minutes_allow_after: Stretch the frame time this many minutes - after
//        When using data time modes, these allowances effectively widen
//        The temporal limits.
//
//  text_off_threshold: Threshold where text disappears. Usees the function:
//         log10(scale_constant /km_distance_across_screen ) + 1.0
//         Use 0 to disable the off feature. (always displayed)
//         Note: scale_constant set in MAIN_PARAM section.      

prod_info = {
   {
    menu_label = "RHI-ticks",
    url = "spdbp:Rhi2Symprod:ticks//$(DATA_HOST)::spdb/rhi/sband",
    data_type = 0,
    render_type = RENDER_ALL,
    on_by_default = FALSE,
    minutes_allow_before = 7.5,
    minutes_allow_after = 7.5,
    text_off_threshold = 0.4,
    request_data_on_zoom = FALSE,
    request_data_on_vert_change = FALSE
  }
  , 
  {
    menu_label = "RHI-label",
    url = "spdbp:Rhi2Symprod:label//$(DATA_HOST)::spdb/rhi/sband",
    data_type = 0,
    render_type = RENDER_ALL,
    on_by_default = FALSE,
    minutes_allow_before = 7.5,
    minutes_allow_after = 7.5,
    text_off_threshold = 0.4,
    request_data_on_zoom = FALSE,
    request_data_on_vert_change = FALSE
  }
//   ,
//   {
//     menu_label = "Surface stations - simple",
//     url = "spdbp:Metar2Symprod:simple//$(DATA_HOST)::spdb/cwb/aws",
//     data_type = 0,
//     render_type = RENDER_LATEST_IN_FRAME,
//     on_by_default = TRUE,
//     minutes_allow_before = 90.0,
//     minutes_allow_after = 0.0,
//     text_off_threshold = 0.5,
//     request_data_on_zoom = FALSE,
//     request_data_on_vert_change = FALSE
//   }
//   ,
//   {
//     menu_label = "Surface stations - labels",
//     url = "spdbp:Metar2Symprod:labels//$(DATA_HOST)::spdb/cwb/aws",
//     data_type = 0,
//     render_type = RENDER_LATEST_IN_FRAME,
//     on_by_default = FALSE,
//     minutes_allow_before = 90.0,
//     minutes_allow_after = 0.0,
//     text_off_threshold = 0.5,
//     request_data_on_zoom = FALSE,
//     request_data_on_vert_change = FALSE
//   }
//   ,
//   {
//     menu_label = "Precip accum",
//     url = "spdbp:Metar2Symprod:precip//$(DATA_HOST)::spdb/cwb/precip",
//     data_type = 0,
//     render_type = RENDER_LATEST_IN_FRAME,
//     on_by_default = FALSE,
//     minutes_allow_before = 70.0,
//     minutes_allow_after = 0.0,
//     text_off_threshold = 0.5,
//     request_data_on_zoom = FALSE,
//     request_data_on_vert_change = FALSE
//   }
};

</SYMPRODS> 

<TERRAIN>
/**********************************************************************
 * TDRP params for Cterrain_P
 * Note: Terrain Fields must be on the same projection as the Display
 **********************************************************************/
///////////// id_label ////////////////////////////////
//
// Label used for legends.
// Type: string
//

id_label = "Terrain";

///////////// terrain_url /////////////////////////////
//
// Used to located gridded terrain data.
// Terrain_URL
//  Note URL is of the form
//   mdvp::[param_file]//host.dom::dir/dir&field_name.
// Type: string
//

// terrain_url = "mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation ";
terrain_url = "";

///////////// height_scaler ///////////////////////////
//
// Conversion from terrain units to local coords.
// Choose a scaler which converts plane height values.
// Type: double
//
//  Hecto-feet (FL) per meter
height_scaler = 0.03280834;

///////////// landuse_url /////////////////////////////
//
// Used to located gridded land use data.
// Land Use URL
//  Note URL is of the form
//   mdvp::[param_file]//host.dom::dir/dir&field_name.
// Type: string
//

//landuse_url = "mdvp::static//$(DATA_HOST):0:marshall/mdv/terrain&Elevation ";
//landuse_url = "";

 ///////////// landuse_colorscale //////////////////////
//
// Color lookup table for Land use.
// Land Use colorscale file name
//  Note: CIDD looks in the colorscale dir, then in the current dir for
//   this file.
// Type: string
//

landuse_colorscale = "land_sea.colors";

///////////// land_use_render_method //////////////////
//
// Land Use rendering style.
// Use Contours only for continious data like elevation.
//
// Type: enum
// Options:
//   RENDER_FILLED_CONT, RENDER_RECTANGLES
//
//
land_use_render_method = RENDER_RECTANGLES;

///////////// earth_color1 ////////////////////////////
//
// Earth s skin color.
// Type: string
//

earth_color1 = "sienna";

///////////// earth_color2 ////////////////////////////
//
// Earth s core color.
// Type: string
//
earth_color2 = "red4";
</TERRAIN>

