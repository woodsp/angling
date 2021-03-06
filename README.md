# Code for assessment of invasion risk at lakes visited by anglers

Invasive species pose a significant threat to ecosystem health and economies of nations across the globe. Freshwater recreational fishing is the largest and growing vector for invader introductions: specifically, because angler activities entangle invasive organisms on fishing gear, boat hulls, and outboard engines, or release non-native species after using them as live bait. Understanding angler movement and behavior can provide critical insight into the most effective implementation of prevention strategies (e.g. watercraft inspection stations, educational signage), thus reducing the introduction, spread and impact of invaders. To date, angler behavior is inferred from sparsely conducted in-person interviews, creels, dairies and mail-in surveys, which tend to produce retrospective data that is limited in time and space and often reveals intentions or attitudes rather than actual behaviors. Moreover, these traditional approaches disproportionately target older anglers and thus fail to engage younger generations whose participation in fishing is rapidly increasing.

Mobile technologies offer a novel opportunity to efficiently collect information on angler behavior at fine spatial and temporal resolutions over broad spatial and temporal scales. Yet, social media and smartphone fishing applications remain an underutilized tool. Anglers are highly active on social media who often geotagg photographs of fish, and fishing applications provide waterbody-specific location of anglers. This can reveal angler behavior that continuous in both space and time, and can provide inexpensive and high-resolution regarding the potential dispersal pathways of aquatic invasive species.

The primary objective of this project is to leverage data from social media and mobile fishing applications to quantify angler activity and movement across the continental United States and assess species invasion risks associated with recreational fishing. Results will directly inform interagency management interventions at both local and landscape scales by quantifying angler movement networks and determining how they change through time. Heavily-used locations of fish activity (i.e., highly-connected network nodes) are prime targets for collaborative regulatory approaches that focus on the implementation of prevention strategies, such as watercraft inspection and cleaning stations to remove invasive species, and educational signage to discourage anglers from releasing live bait into waterbodies.


## Workflow

Gather the required inputs

Run `spatialjoin.r`
  requires angling events (iBobber_Jan25_2019.csv)
  requires pre-processed NHD of waterbodies with correct FCode (NHD_H_National_GDB_Waterbody_39000-46599.gpkg
  requires US state polygons (cb_2017_us_state_20m)
  returns waterbody x state linking table
  returns angling event to waterbody linking table
  returns angling events table with waterbody metadata columns

Run `clustering.r`


