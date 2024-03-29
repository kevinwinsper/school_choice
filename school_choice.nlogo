extensions [ gis R vid shell ]

globals [ lbbd-msoa lbbd-lsoa lbbd-schools schoolradii directory exp-index asp-moran-i asp-moran-i-p att-moran-i att-moran-i-p inc-moran-i inc-moran-i-p ga-m ga-r ga-p gmx-m gmx-r gmx-p gmn-m gmn-r gmn-p amx-m amx-r amx-p amn-m amn-r amn-p _recording-save-file-name]

breed [parents parent]
breed [schools school]

parents-own
[
  strategy ;indicator of which ranking method agent used
  rankings ;list of schools that are worthy of ranking (transfered into specific ranks rank1, rank2... etc)
  distances ;list of schools in ascending of order of distance THIS WILL NEED TO BE UPDATED SDURING MODEL RUN WHEN SCHOOL CHANGES ARE IMPLEMENTED

  allocated-school ;school allocated
  allocated-distance ;distance to allocated school
  aspiration ;aspiration level of parent

  hhold-income ;household income of parent

  success-by-rank  ;rank of allocated school (1 is top rank, i.e. most preferred)
  success-rank1  ;true if allocated to top-ranked school, else false (or -1 if not allocated)
  success-by-aspiration   ;true if allocated school GCSE-score > aspiration at time of allocation

  considered ; list of schools considered acceptable given aspiration
  catchment ; list of schools the parent believes they are in the catchment of (given last 3 years catchments) - 3? or memory?
  avoided ; list of schools the parent wishes to avoid because school GCSE-score is some level below their aspiration
  expensive ; list of schools the parent wishes to avoid because they may not be able to afford house in their catchment area

  child-age ;age of child. children apply for school when 10 years old (therefore only rank and allocate when child = 10)
  child-attainment ; initially set to parent aspiration, can increase during school years

  want-to-move ;true or false - does this parent want to move?
  have-moved ; true or false - has this parent moved? if so cannot move again
  move-target ;if parent wants to move, this is the preferred school
  initialHome ;patch where Parent initially lives
  newHome ;patch where Parent moves to
]

schools-own
[
  school-type ;state or new?
  id ;school id

  GCSE-score ;measure of school quality
  ;GCSE-z-score ;z-score of school GCSE score
  GCSE-scores ; list containing last MEMORY years GCSE-scores
  value-added; % by which incoming students aspiration is increased year-on-year to y11
  mean-aspiration
  mean-attainment ;added v6

  places ;pupil places available at the school for each year
  last-growth ;last tick in which school expanded
  y7pupils ;number of pupils allocated this year
  all-pupils ;total number of pupils in the school

  allocated  ;list holding allocated parents
  all-applicants ;list holding all applicants for this year with given criteria (see below)
  applicants ;list holding applicants for this year - changes through application process (allocate-places)
  app-ratio ;ratio of applications to places
  max-distance ;distance of farthest allocated parent
  max-distances ;list of last Parent-Memory years' max distances
  mean-distance ; mean distance of parents
  mean-distances ;list of last Parent-Memory years' mean distances

  y7Parents ;list of parents with children in year 7 (initially this is simply a value specifying that year's GCSE score, through time becomes a list which is updated each year)
  y8Parents ;list of parents with children in year 8 (initially this is simply a value specifying that year's GCSE score, through time becomes a list which is updated each year)
  y9Parents ;list of parents with children in year 9 (initially this is simply a value specifying that year's GCSE score, through time becomes a list which is updated each year)
  y10Parents ;list of parents with children in year 10 (initially this is simply a value specifying that year's GCSE score, through time becomes a list which is updated each year)
  y11Parents ;list of parents with children in year 11 (initially this is simply a value specifying that year's GCSE score, through time becomes a list which is updated each year)

  catchmentPatches ; patches within school's max-distance with no parents in them
  mean-catchment-price
  mean-catchment-prices ;list of last Parent-Memory years' mean household price
]

patches-own
[
  centroid ;centroid of msoa
  msoa ;msoa that the patch belongs to
  mean-house-price ;mean house price of the msoa that the patch belongs to
  house-price ;value of the house
  within-lbbd ;whether the patch is within lbbd boundary
  mean-hhold-income ;mean household income of the msoa that the patch belongs to
  original-price ;orginal price of the house at model initialisation
  current-price ;current price of the house at model initialisation
  price-updated ;whether the price has been updated
  p-aspiration ;aspiration of the parent on the patch
  p-attainment ;attainment of the parent on the patch
  p-hhold-income ;household income of the parent on the patch
  location-value
]

;;----------------------------------
;;SETUP
;;----------------------------------

to setup

  clear-all
  random-seed seed

  setup-map
  setup-Patches
  setup-Schools
  setup-Parents
  if(calc-Moran?) [ setup-R ]
  reset-ticks
  plotting

end

to setup-map

  set lbbd-msoa gis:load-dataset "/Users/kevinwinsper/LocalDoc/GitHub/school_choice/data/LBBD_MSOA_HI.shp"
  gis:set-world-envelope (gis:envelope-of lbbd-msoa)
  gis:set-drawing-color white
  gis:draw lbbd-msoa 1

end

to setup-Parents
  print "setting up parents"

  let addedParents 0
  while [ addedParents < 7000]
  [
  ask one-of patches with [within-lbbd = true]
    [
      let mean-income [mean-hhold-income] of self
      if(not any? turtles-here) ; make sure parents do not occupy school patch
      [
        sprout-parents 1
        [
          set color grey
          set size 1
          set shape "circle"
          facexy 0 0

          set hhold-income log-normal mean-income 4500

          set aspiration -1
          while [aspiration < 30 or aspiration > 100]
          [
            set aspiration random-normal 50 20
          ]
          set child-attainment aspiration

          set child-age (floor ((addedParents + 1) / (7000 / 7))) + 9

          set allocated-school 0

          set have-moved false
          set want-to-move true
          set initialHome myself
          set newHome nobody

          set rankings []

          set success-by-rank -1
          set success-rank1 -1
          set success-by-aspiration -1
          set strategy -1
        ]
          set addedParents addedParents + 1
      ]
    ]
  ]
  if(any? parents with [child-age = 16])
  [
    ask parents with [child-age = 16] [ set child-age 9 ]
  ]

end

to setup-Schools
  print "setting up schools"

  set lbbd-schools gis:load-dataset "/Users/kevinwinsper/LocalDoc/GitHub/school_choice/data/LBBD_secondary.shp"
  foreach gis:feature-list-of lbbd-schools [ feature ->
    ask patches gis:intersecting feature [
      set plabel 1
      set plabel-color black
      sprout-schools 1
      [
        set size 3
        set shape "house"
        set school-type "state"

        set GCSE-score -1

        while [GCSE-score < 0 or GCSE-score > 100]
        [
          if(Initial-School-GCSE-Distribution = "uniform") [ set GCSE-score 1 ]
          if(Initial-School-GCSE-Distribution = "normal") [ set GCSE-score random-normal 50 20 ]
          if(Initial-School-GCSE-Distribution = "negative-exponential") [ set GCSE-score random-exponential 25 ]
        ]

        set GCSE-scores []

        set value-added -2
          if(School-Value-Added)
          [
            while [value-added < -1 or value-added > 1 ]
            [
              if(School-Value-Added-Distribution = "uniform") [ set value-added 0.1 ]
              if(School-Value-Added-Distribution = "normal") [ set value-added random-normal 0 0.3 ]

              if(value-added < 0) [ set value-added (value-added * -1) ] ;do not allow value-added to be negative

              ;to check influence of increased value-added
              ;set value-added value-added + 0.1
            ]
          ]

          set y7Parents []
          set y8Parents []
          set y9Parents []
          set y10Parents []
          set y11Parents []

          set places SchoolSize
          set id (count schools - 1)      ;first school has id of zero
          set color (count schools - 1) * 10 + 15  ;first school will be red
                                                     ;set size 0

          set max-distances []
          set mean-distances []
          set allocated []
          set applicants []

          set mean-distance 0

      ]
    ]
  ]
  ; code to relocate school 3
  let newLocation3 patch 16 -49
  let newLocation9 patch -45 -34
;  ask school 3
;  [
;    move-to newLocation3
;  ]
;  ask school 9
;  [
;    move-to newLocation9
;  ]

end

to setup-Patches

  ask patches [ set within-lbbd false ]
  foreach gis:feature-list-of lbbd-msoa [ feature ->
    ask patches gis:intersecting feature [
      set within-lbbd true
      set centroid gis:location-of gis:centroid-of feature
      set msoa gis:property-value feature "MSOA11CD"
      set mean-house-price gis:property-value feature "HPRICE"
      set house-price log-normal mean-house-price 4000
      set mean-hhold-income gis:property-value feature "HHOLDINC"
      set original-price house-price
      set current-price original-price
      set price-updated false
    ]
  ]

end

to set-parent-school-distance

  set distances sort-by [ [?1 ?2] -> distance ?1 < distance ?2 ] schools

end

; Lognormal distribution
to-report log-normal [mu sigma]
  let beta ln ( 1 + ( ( sigma ^ 2 ) / ( mu ^ 2 ) ) )
  let M ln (mu) - ( beta / 2 )
  let S sqrt beta
  let x exp ( random-normal M S )
  report x
end

;;----------------------------------
;;SIMULATION
;;----------------------------------
to go

  if(ticks = 0)
  [
    with-local-randomness
    [
      if(Export-Summary-Data or Export-World-Data or Export-Movie)
      [
        ;create a new directory for this model run
        set directory "/Users/kevinwinsper/LocalDoc/GitHub/school_choice/export"
        set-current-directory directory
        shell:cd directory
        print directory
        print shell:pwd

        ;create a dummy file to check what new name to give the directory (checked in next-index procedure)
        let dummy "Experiment"
        let suffix ".txt"
        set exp-index next-index dummy suffix
        let filename (word shell:pwd "/" dummy exp-index suffix)
        file-open filename
        write-experiment-data
        file-close

        ;set the new directory name and create
        set directory (word shell:pwd "/" dummy exp-index) ;set
        show(shell:exec "mkdir" directory) ;create
        set-current-directory directory ;update
        shell:cd directory ;update

      ]

      if(Export-Summary-Data) [ ExportSummaryData_header ]
      if(Export-World-Data) [ ExportWorldData_initial ]
      if(Export-Movie)
      [
        set _recording-save-file-name "movie.mov"
        vid:start-recorder
        vid:record-view
      ]
    ]


    ask parents [set-parent-school-distance]
    set-SchoolCatchments
    calc-mean-catchment-price
    rank-Schools
    move
  ]


  show "Adding new parents and aging children"
  age-children
  age-PupilCohorts ;move cohorts of students up one year
  add-NewParents ;add new parents for this tick
  set-schoolCatchments ;find patches in catchment available to move into this tick (do this after adding new parents)
  update-HousePrice ;update house price
  calc-mean-catchment-price ;calculate mean house price of houses in each schools catchment area


  show "Parents ranking schools"
  rank-Schools  ;parents set their school ranking

  show "Allocating places"
  allocate-Places ;schools allocate places to parents with child-age = 10

  show "Moving agents"
  move ;parents with child-age = 9 potentially move to a better location

  if(ticks > 80 and calc-Moran? = true) [set-patch-attributes]

  show "Updating Schools"
  calc-catchment-size  ;schools calculate mean and max allocated distance of their parents
  update-SchoolGCSE ;update schools' GCSE-score


  with-local-randomness
  [
    show "Plotting etc"
    check-success
    update-colours
    plotting

    if(Export-Summary-Data)
    [
      calc-Moran
      ;calc-relationships
      ExportSummaryData
    ]

    if(Export-World-Data) [ ExportWorldData ]
    if(Export-Movie) [ vid:record-view ]
  ]

  tick

  if(ticks = run-length)
  [
    if(Export-Movie) [ vid:save-recording _recording-save-file-name ]
    if(Export-Summary-Data)
    [
      export-worldview
      ;ZipParents_Data
    ]
    stop
  ]


end

;update house price of each patch
to update-HousePrice
  let tempSchools sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] schools
  let thisRank 0

  ask patches with [within-lbbd = true]
  [
    set price-updated false
  ]

  while[thisRank < 10]
  [
    let tempPatches [catchmentPatches] of item thisRank tempSchools
    ifelse (thisRank < 1)
    [
      ask patch-set tempPatches
      [
        set current-price current-price * 1.1
        set price-updated true
      ]
    ]
    [
      ifelse (thisRank < 2)
      [
        ask patch-set tempPatches
        [
          set current-price current-price * 1.08
          set price-updated true
        ]
      ]
      [
        ifelse (thisRank < 5)
        [
          ask patch-set tempPatches
          [
            set current-price current-price * 1.04
            set price-updated true
          ]
        ]
        [
          ifelse (thisRank < 8)
          [
            ask patch-set tempPatches
            [
              set current-price current-price * 1.02
              set price-updated true
            ]
          ]
          [
            ask patch-set tempPatches
            [
              set current-price current-price * 1.005
              set price-updated true
            ]
          ]
        ]
      ]
    ]
    set thisRank thisRank + 1
  ]
  ask patches with [within-lbbd = true and price-updated = false]
  [
    set current-price current-price * 1.01
    set price-updated true
  ]

end

to set-SchoolCatchments
  ;creates a list of patches within the school catchment that are free to move into this timestep

  ;if this is the first tick (ticks = 0) max-distances will not have been set
  ifelse(ticks > 0)
  [
    ask schools
    [
      set catchmentPatches []
      set catchmentPatches sort patches with [ distance myself < min [ mean-distances ] of myself and not any? turtles-here and within-lbbd = true ]
    ]
  ]

  [
    ask schools
    [
      set catchmentPatches []
      set catchmentPatches sort patches with [ distance myself < (2 * world-width) and not any? turtles-here and within-lbbd = true ]
    ]
  ]
end

to calc-mean-catchment-price
  ifelse(ticks > 0)
  [
    ask schools
    [
      ifelse(empty? catchmentPatches)
      [
        set mean-catchment-price 1500000  ;if places were allocated find the mean distance of allocated parents
      ]
      [
        set mean-catchment-price mean [current-price] of patches with [ distance myself < min [ mean-distances ] of myself and not any? turtles-here and within-lbbd = true ] ;if places were allocated find the mean distance of allocated parents
      ]
      set mean-catchment-prices lput mean-catchment-price mean-catchment-prices ;add the mean distance to the list of mean distances
      if(length mean-catchment-prices > Parent-Memory) [ set mean-catchment-prices remove-item 0 mean-catchment-prices ]  ;only keep track of last *Memory* years distances
    ]
  ]

  [
    ask schools
    [
      set mean-catchment-prices []
      ifelse(not empty? catchmentPatches)
      [
        set mean-catchment-price 1500000  ;if places were allocated find the mean distance of allocated parents
      ]
      [
        set mean-catchment-price mean [current-price] of patches with [ distance myself < (2 * world-width) and not any? turtles-here and within-lbbd = true ] ;if places were allocated find the mean distance of allocated parents
      ]
      set mean-catchment-prices lput mean-catchment-price mean-catchment-prices ;add the mean distance to the list of mean distances
      if(length mean-catchment-prices > Parent-Memory) [ set mean-catchment-prices remove-item 0 mean-catchment-prices ]  ;only keep track of last *Memory* years distances
    ]
  ]
end

to rank-Schools

  let reporter nobody

  ask parents with [child-age = 10 or (have-moved = false and child-age = 9)] ;two types of parents need to rank schools
  [
    ;parents calculate which catchments they are in by comparing their distance to each school with mean of last [memory] years of mean-distances
    set rankings []

    ifelse(ticks >= 1) ;mean-distances (catchment) will not have been calculated in first step
    [  set catchment schools with [ distance myself < min [mean-distances] of self ] ] ;creates agentset of schools parent believes they are in the catchment for
    [  set catchment schools with [ distance myself < (world-width * 2) ]  ] ;creates agentset of schools parent belives they are in the catchment for

    if(any? catchment) [ set catchment sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] catchment ] ;changes agentset to list and ranks on GCSE score (descending)

    set considered schools with [ GCSE-score >= [aspiration] of myself ] ;creates agentset of school parent believes are good enough
    if(any? considered) [ set considered sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] considered ] ;changes agentset to list and ranks on GCSE score (descending)

    set avoided schools with [ GCSE-score < ([aspiration] of myself * Avoided-Threshold) ] ;avoided threshold must be >0 and <1
    if(Avoid-Schools = true) ;if not considering avoided schools, avoided is not changed to a list and therefore is assumed empty below
    [
      if(any? avoided) [ set avoided sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] avoided ] ;changes agentset to list and ranks on GCSE score (descending)
    ]

    set expensive schools with [ min [mean-catchment-prices] of self > [hhold-income] of myself * Price-Income-Ratio ]
    if(Move-Best = false) ;if parents also consider affordability instead of only considering moving to the best school
    [
      if(any? expensive) [ set expensive sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] expensive ] ;changes agentset to list and ranks on GCSE score (descending)
    ]


    ;ranking for moving (when child-age = 9) is different for ranking for allocating (when child-age = 10)
    ;for moving, parent ranks solely on GCSE score (and then evaluate which school catchments they are in and whether this is acceptable)
    ;for allocating, parents consider GCSE score AND distance


    ;rank for moving
    ;parents try to position themselves in considered (preferred) school catchments but not in avoided school catchments

    ;check if I want to move (don't move if I am in a considered school catchment AND am not in an avoided school catchment)
    if(child-age = 9 and want-to-move = true and have-moved = false)
    [
      let considered-present false
      let avoided-present false

      if(is-list? catchment) ;if is a list it has schools in it
      [
        if(is-list? considered)  ;if is a list it has schools in it
        [
          foreach catchment [ [?1] -> if(member? ?1 considered and [school-type] of ?1 != "new") [ set considered-present true ] ] ;only consider state school catchments (will always be in the catchment of new schools which do not select on distance)
        ]

        if(is-list? avoided) ;if is a list it has schools in it
        [
          foreach catchment [ [?1] -> if(member? ?1 avoided) [ set avoided-present true ] ]
        ]

        if(considered-present = true) [ set want-to-move false ]  ;if there is a considered school in my catchment I will not move (move function will not execute if moved is true)
        if(avoided-present = true) [ set want-to-move true ]  ;but if there is also an avoided school in my catchment, I will try to move
      ]


      ;if I still want to move
      if(child-age = 9 and want-to-move = true)
      [
        ;if there are considered schools, rank considered schools on GCSE-score
        ifelse(is-list? considered)
        [
          set rankings sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] considered  ;considered schools will never be avoided schools

          ;if I want to move and avoided is a list (i.e. there is something in it) I must be in the catchment of one of the avoided schools
          ;so add as many non-avoided schools as possible, in descending order of GCSE score
          if(is-list? avoided)
          [
            let tempSchools sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] schools

            ifelse(Move-Best = false)
            [
              ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
              foreach tempSchools
              [ [?1] ->
                if(not member? ?1 rankings and not member? ?1 avoided and not member? ?1 expensive) [ set rankings lput ?1 rankings ]
              ]
            ]

            [
              foreach tempSchools
              [ [?1] ->
                if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
              ]
            ]
          ]
        ]

        ;if there are no considered schools, rank all schools by GCSE-score but do not include avoided
        [
          set rankings [] ;check there's nothing in rankings
          let tempSchools sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] schools

          ifelse(is-list? avoided) ;will be a list if it has something in it
          [
            ifelse(Move-Best = false)
            [
              ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
              foreach tempSchools
              [ [?1] ->
                if(not member? ?1 avoided and not member? ?1 expensive) [ set rankings lput ?1 rankings ]
              ]
            ]

            [
              foreach tempSchools
              [ [?1] ->
                if(not member? ?1 avoided) [ set rankings lput ?1 rankings ]
              ]
            ]
          ]

          ;if there is nothing in avoided add all schools
          [
            ifelse(Move-Best = false)
            [
              ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
              foreach tempSchools
              [ [?1] ->
                if(not member? ?1 expensive) [ set rankings lput ?1 rankings ]
              ]
            ]

            [
              set rankings tempSchools
            ]
          ]
        ]
        ;if(rankings = []) [set want-to-move false] ; if parent cannot afford school in any catchment area
      ]
    ]




    ;rank for allocating
    if(child-age = 10)
    [
      ifelse(is-agentset? catchment) ;if still an agentsent (and not converted to list) it is empty
      [
        ifelse(is-agentset? considered) ;if still an agentsent (and not converted to list) it is empty
        [
          ;catchment and considered are both empty (i.e. there are no schools with GCSE-score higher than my aspiration and I am not near any school)
          ;strategy here is to rank all schools on distance, but do not include avoided in the rankings

          set strategy 1

          ifelse(is-list? avoided) ;will be a list if it has something in it
          [
            set strategy 2

            let tempSchools sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools

            foreach tempSchools
            [ [?1] ->
              ;if this school is not in the avoided list, add it to the rankings list
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]
          ]

          ;if nothing in avoided add all schools to ranking based on distance
          [ set rankings sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools ]
        ]

        [
          ;catchment is empty, considered is not empty (i.e. there are schools with GCSE-score higher than my aspiration and but I am not near any of them [or any school])
          ;strategy here is to rank considered schools on distance, then all schools on distance but do not include avoided

          set strategy 3

          set rankings sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] considered  ;considered schools will never be avoided schools
          let tempSchools sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools

          ifelse(is-list? avoided) ;will be a list if it has something in it
          [
            set strategy 4

            ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
            foreach tempSchools
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]
          ]

          [
            ;else if nothing in avoided, add tempSchools to end of rankings
            foreach tempSchools
            [ [?1] ->
              if(not member? ?1 rankings) [ set rankings lput ?1 rankings ]
            ]
          ]
        ]
      ]

      [
        ;catchment has schools in it [always full if distance-allocation = T]

        ifelse(is-agentset? considered) ;if still an agentsent (and not converted to list) it is empty
        [
          ;catchment is not empty but considered is (i.e. there are schools I will likely be allocated but I do not consider them good enough)
          ;strategy here is to rank catchment on GCSE-score (descending), then all other schools on distance, but do not include avoided schools
          ;[justfied to still rank on distance when distance-allocatin = F as parents still like their kids to go to school nearby]

          set strategy 5

          let tempCatchment catchment ;already sorted by GCSE
          let tempSchools sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools


          ;reporting code for debugging
          ;if(reporter = nobody)
          ;[
          ;  set reporter self
          ;  show tempCatchment
          ;  show tempSchools
          ;  show rankings
          ;]

          ifelse(is-list? avoided) ;will be a list if it has something in it
          [
            set strategy 6

            ;add schools from tempCatchment to rankings, unless it is in avoided
            foreach tempCatchment
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]

            ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
            foreach tempSchools
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]
          ]

          ;else if nothing in avoided, add tempCatchment and tempSchools to end of rankings
          [
            foreach tempCatchment [ [?1] -> if(not member? ?1 rankings) [ set rankings lput ?1 rankings ] ]
            foreach tempSchools [ [?1] -> if(not member? ?1 rankings) [ set rankings lput ?1 rankings ] ]
          ]

          ;reporting code for debugging
          ;if(reporter = self)
          ;[
          ;  show tempCatchment
          ;  show tempSchools
          ;  show rankings
          ;]
        ]


        [
          ;neither catchment nor considered is empty
          ;rank catchment on GCSE-score (descending), then considered on distance, then all other schools on distance but do not include avoided

          set strategy 7

          let tempCatchment catchment ;already sorted by GCSE
          let tempConsidered sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] considered
          let tempSchools sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools



          ifelse(is-list? avoided) ;will be a list if it has something in it
          [
            set strategy 8

            ;if this school is not in the avoided list, add it to the rankings list
            foreach tempCatchment
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]

            ;add schools from tempConsidered to end of rankings, unless it is already in rankings or is in avoided
            foreach tempConsidered
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]

            ;add schools from tempSchools to end of rankings, unless it is already in rankings or is in avoided
            foreach tempSchools
            [ [?1] ->
              if(not member? ?1 rankings and not member? ?1 avoided) [ set rankings lput ?1 rankings ]
            ]
          ]

          ;else if nothing in avoided, add tempRankings, tempConsidered and tempSchools to end of rankings
          [
            foreach tempCatchment [ [?1] -> if(not member? ?1 rankings) [ set rankings lput ?1 rankings ] ]
            foreach tempConsidered [ [?1] -> if(not member? ?1 rankings) [ set rankings lput ?1 rankings ] ]
            foreach tempSchools [ [?1] -> if(not member? ?1 rankings) [ set rankings lput ?1 rankings ] ]
          ]
        ]
      ]

    ]


    if(not empty? rankings)
    [
      ;rankings may be too long, if so remove items
      if(length rankings > Number-of-Ranks)
      [
        set rankings reverse rankings ;its faster to remove from the front (and don't need to know length of rankings)

        while[length rankings > Number-of-Ranks]
        [
          set rankings remove-item 0 rankings
        ]

        set rankings reverse rankings
      ]
    ]



    ;rankings always needs to be of length Number-of-Ranks to prevent out-of-list search in allocate-Places function (so add dummy values to fill list)
    if(length rankings < Number-of-Ranks)
    [
      let thisRank length rankings

      while[thisRank < Number-of-Ranks]
      [
        set rankings lput -1 rankings
        set thisRank thisRank + 1
      ]

      ;set rankings empty-rankings
    ]


    ;reporting code for debugging
    ;if(reporter = self)
    ;    [
    ;      show rankings
    ;      set reporter true
    ;    ]

  ]

end

to move

  ;ask parents [ if(child-age = 10 and have-moved = false) [set want-to-move false ] ] ;don't move once child-age > 10

  let movers no-turtles

  set movers parents with [child-age = 9 and want-to-move = true and have-moved = false]

  ask schools
  [
    ifelse(Move-Closest = true)
    [ set catchmentPatches sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] catchmentPatches ]
    [ set catchmentPatches shuffle catchmentPatches ]
  ]



  ask movers
  [
    let thisRank 0

    while[thisRank < Number-of-Ranks]
    [
      if(newHome = nobody)
      [
        let thisSchool item thisRank [rankings] of self

        ;check if I think I am already in this school's catchment
        ;if so, only move if can move closer AND to a patch with greater value than current patch value (but still less than my aspiration)
        ;no need to check other schools in rankings once this has been done...

        ;if not, and I am checking this school, it must be higher ranked than the school catchment I am currently in so move there regardless of patchvalue (although still less than my aspiration)

        if(is-agent? thisSchool and [school-type] of thisSchool != "new") ;ranking may contain non-Schools (i.e. -1 values) - do not try to move near to 'new' schools (no need as they do not allocate on distance. Also, catchment is entire world so make little point in moving anywhere within the world)
        [
          if(not empty? [catchmentPatches] of thisSchool) ;if this school has available patches in its catchment
          [
            let houseValue-max hhold-income * Price-Income-Ratio

            ;for each available patch in the school catchment, check if patchValue is less than parent's aspiration
            let tempCatchment patch-set [catchmentPatches] of thisSchool
            ;set tempCatchment tempCatchment with [house-price < houseValue-max or patchValue = -1]
            set tempCatchment tempCatchment with [house-price < houseValue-max]

            ;if parent should maximise proximity to school
            ifelse(Move-Closest = true)
            [
              if(any? tempCatchment)
              [
                set newHome min-one-of tempCatchment [distance thisSchool]
                set move-target thisSchool
              ]
            ]

            ;else move to any patch
            [
              if(any? tempCatchment)
              [
                set newHome one-of tempCatchment
                set move-target thisSchool
              ]
            ]

            if(newHome != nobody)
            [
              ;show initialHome
              ask schools
              [
                ;if(member? [newHome] of myself catchmentPatches) [ show word "removing newHome: " [newHome] of myself ]
                ask [newHome] of myself [if(any? parents-here) [ show "error - already a parent at new home!" ] ]
                set catchmentPatches remove [newHome] of myself catchmentPatches
              ]

              ask schools [ add-available-home-to-catchment [initialHome] of myself ]
            ]
          ]
        ]

        if(newHome != nobody)
        [
          move-to newHome

          if(any? other parents-here) ;Error check
          [
            let pid [who] of other parents-here
            show word "Error: another parent already here, agent:" pid
          ]

          set have-moved true
          ask(initialHome)
          [

            if(ticks > 80 and calc-Moran? = true) [ set-location-value ] ;this works because parent can only move once.
            if(any? parents-here) [ show "error - still a parent here!" ]
          ]
        ]
      ]

      set thisRank thisRank + 1
    ]

    set-parent-school-distance ;update distances list
  ]

end

to add-available-home-to-catchment [ availableHome ]

  let add false
  ifelse(ticks = 0) ;if ticks = 0, mean-distances will be empty causing an error when checked with an if statement (and catchmentPatches will contain all patches so always add when ticks = 0)
  [ set add true ]
  [ if(distance availableHome < min mean-distances) [ set add true ] ]

  if(add)
  [
    ;show word "adding availableHome: " availableHome

    set catchmentPatches sentence availableHome catchmentPatches

    ifelse(Move-Closest = true)
    [ set catchmentPatches sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] catchmentPatches ]
    [ set catchmentPatches shuffle catchmentPatches ]
  ]

end

to age-children
  ask parents
  [
    set child-age child-age + 1
  ]

  ;kill parents with children that have left school
  ask parents with [ child-age = 16 ]
  [
    die
  ]

end

to age-PupilCohorts

  ;move each cohort of students up one year (by replacing parent lists with previous year's parent list)
  ;create y7Parents from the allocated list for the school
  ;don't age cohorts in the first tick because there no pupils have been allocated yet
  if(ticks > 0)
  [
    ask schools
    [
      set y11Parents []
      foreach y10Parents [ [?1] -> set y11Parents fput ?1 y11Parents ]

      set y10Parents []
      foreach y9Parents [ [?1] -> set y10Parents fput ?1 y10Parents ]

      set y9Parents []
      foreach y8Parents [ [?1] -> set y9Parents fput ?1 y9Parents ]

      set y8Parents []
      foreach y7Parents [ [?1] -> set y8Parents fput ?1 y8Parents ]

      set y7Parents []
      foreach allocated [ [?1] -> set y7Parents fput ?1 y7Parents ]  ;copy allocated to y7Parents (allocated are parents with child-age = 10 last year, now child-age = 11)
    ]
  ]

end

to add-NewParents

  let addedParents 0

  while [addedParents < 7000 / 7]
  [
    let sprouting-patch nobody

    ask one-of patches with [within-lbbd = true]
    [
      if(not any? turtles-here)
      [
        let house-price-here [current-price] of self
        create-parent
        ask parents-here
        [
          set hhold-income house-price-here / (Price-Income-Ratio - 0.5)
          while [aspiration < 0 or aspiration > 100]
            [
              set aspiration random-normal 50 20
            ]
          set child-attainment aspiration
        ]
        set addedParents addedParents + 1
      ]
    ]

  ]

end

to create-parent

   sprout-parents 1
     [
       set color grey
       if(Show-Unallocated = false) [ set hidden? true ]
       set size 1
       set shape "circle"
       facexy 0 0

       set aspiration -1

       set child-attainment aspiration

       set child-age 9

       set allocated-school 0

       set have-moved false
       set want-to-move true
       set initialHome myself
       set newHome nobody

       set rankings []
       set-parent-school-distance

       set success-by-rank -1
       set success-rank1 -1
       set success-by-aspiration -1
       set strategy -1
     ]

end

to allocate-Places

  ask schools
  [
    set all-applicants []
    set all-applicants parents with [child-age = 10 and member? myself rankings and allocated-school = 0]
    set app-ratio (count all-applicants) / places

    set y7pupils 0 ;re-set number of year 7 pupils for this year of allocations
    set allocated []
  ]


    ;all schools allocate applicants that ranked them highest first (potentially using distance if there are too many applicants)
    ;then school allocate applicants that ranked them second (again, potentially using distance if there are too many applicants), third, fourth, etc.
    ;schools that have remaining places after all ranked preferences have been allocated, allocate remaining places on distance or randomly

    let thisRank 0

    while[thisRank < Number-of-Ranks]
    [
      ask schools
      [
        if(y7pupils < places)
        [
          set applicants parents with [child-age = 10 and item thisRank rankings = myself and allocated-school = 0]   ;add parents to applicants list if they have ranked this school highest
          if(school-type = "state")
          [
            set applicants sort-by [ [?1 ?2] -> distance ?1 < distance ?2 ] applicants  ;sort the list of applicants, closest is first in list NOTE: this sorting changes agentset to a list
          ]

          if(school-type = "new") [ set applicants sort-by [ [?1 ?2] -> [child-attainment] of ?1 > [child-attainment] of ?2 ] applicants ]  ;sort the list of applicants, highest attainment is first in list NOTE: this sorting changes agentset to a list

          ifelse(length applicants > (places - y7pupils))  ;if there are more applicants than places available at the school, allocate by distance
          [
            while [y7pupils < places]  ;while not all places have been allocated
            [
              ask first applicants [ set allocated-school myself ]  ;allocate the closest applicant
              ;ask first applicants [ set allocated-school myself set rankings empty-rankings ]  ;allocate the current 'best' applicant
              set allocated lput first applicants allocated          ;add this current 'best' applicant to the allocated list
              set applicants remove-item 0 applicants                ;remove this current 'best' applicant from the applicants list
              set y7pupils y7pupils + 1                              ;increase number of places allocated by 1
            ]
          ]

          ;if there are more places than applicants allocate all applicants a place
          [
            foreach applicants
            [ [?1] ->
              ask ?1 [set allocated-school myself ]
              ;ask ? [set allocated-school myself set rankings empty-rankings ]
              set allocated lput ?1 allocated          ;add this applicant to the allocated list
            ]
            set y7pupils (y7pupils + length applicants)
            ;set applicants []
          ]
        ]
      ]

      set thisRank thisRank + 1
    ]


    ;finally, for schools with unallocated places, allocate unallocated applicants by distance
    ;first allocated to parents of which this school is closest, then second closest etc.
    let thisDist 0

    while[thisDist < 10] ;number of schools 10
    [
      ask schools with [y7pupils < places]
      [
        set applicants parents with [child-age = 10 and allocated-school = 0 and item thisDist distances = myself]
        set applicants sort-by [ [?1 ?2] -> distance ?1 < distance ?2 ] applicants

        ifelse(length applicants > (places - y7pupils))
        [
          while [y7pupils < places]
          [
            ask first applicants [ set allocated-school myself ]
            ;ask first applicants [ set allocated-school myself set rankings empty-rankings ]
            set allocated lput first applicants allocated          ;add this closest applicant to the allocated list
            set applicants remove-item 0 applicants                ;remove this closest applicant from the applicants list
            set y7pupils y7pupils + 1
          ]
        ]

        ;if there are more places than applicants allocate all applicants a place
        [
          without-interruption
          [
            foreach applicants
            [ [?1] ->
              ask ?1 [set allocated-school myself ]
              ;ask ? [set allocated-school myself set rankings empty-rankings ]
              set allocated lput ?1 allocated          ;add this applicant to the allocated list
            ]
            set y7pupils (y7pupils + length applicants)
            ;set applicants []
          ]
        ]
      ]

      set thisDist thisDist + 1
    ]



    ask schools
    [
      set all-pupils (length y7parents + length y8parents + length y9parents + length y10parents + length y11parents)
    ]

    ask parents with [child-age = 10 ]
    [
      set allocated-distance distance allocated-school
    ]


end

to calc-catchment-size

  ;calculates mean and max allocated parent distances

  ifelse(ticks > 0) ;initially schools have no parents
  [
    ask schools with [school-type = "state"]
    [
      let all-parents parents with [ allocated-school = myself ]

      ifelse(not any? all-parents)
      [
        set max-distance -1 ;if no places were allocated set a no data value
        set max-distances lput 0 max-distances ;add the max distance to the list of max distances
        if(length max-distances > Parent-Memory) [ set max-distances remove-item 0 max-distances ]  ;only keep track of last *Memory* years distances

        set mean-distance -1 ;if no places were allocated set a no data value
        set mean-distances lput 0 mean-distances ;add the mean distance to the list of mean distances
        if(length mean-distances > Parent-Memory) [ set mean-distances remove-item 0 mean-distances ]  ;only keep track of last *Memory* years distances
      ]
      [
        set max-distance max [allocated-distance] of all-parents ;if places were allocated find the distance of the furthest allocated parent
        set max-distances lput max-distance max-distances ;add the max distance to the list of max distances
        if(length max-distances > Parent-Memory) [ set max-distances remove-item 0 max-distances ]  ;only keep track of last *Memory* years distances

        set mean-distance mean [allocated-distance] of all-parents  ;if places were allocated find the mean distance of allocated parents
        set mean-distances lput mean-distance mean-distances ;add the mean distance to the list of mean distances
        if(length mean-distances > Parent-Memory) [ set mean-distances remove-item 0 mean-distances ]  ;only keep track of last *Memory* years distances
      ]
    ]

    ask schools with [school-type = "new"]
    [
      set max-distances fput (2 * world-width) max-distances
      set mean-distances fput (2 * world-width) mean-distances
    ]

  ]

  ;instead just set catchment to larger than the world
  [
    ask schools
    [
      set max-distances fput (2 * world-width) max-distances
      set mean-distances fput (2 * world-width) mean-distances
    ]
  ]

end

to update-SchoolGCSE

  ;first update child-attainment for this year
  ask schools
  [
    ;show mean-aspiration
    set-child-attainment y7Parents
    set-child-attainment y8Parents
    set-child-attainment y9Parents
    set-child-attainment y10Parents
    set-child-attainment y11Parents
  ]

  ;set mean and max child-attainment
  ask schools
  [
    let myParents (turtle-set y7parents y8parents y9parents y10parents y11parents)

    ifelse(any? myParents)
    [
      set mean-aspiration mean [aspiration] of myParents
      set mean-attainment mean [child-attainment] of myParents
    ]
    [
      set mean-aspiration 0
      set mean-attainment 0
    ]
  ]

  ;only start updating GCSE-score once first cohort of allocated students has reached year 11 (before this y11Parents will be empty)
  if(ticks > 5)
  [
    ask schools
    [
      let sumAttainment 0

      if(not empty? y11Parents)
      [
        set sumAttainment sum [child-attainment] of turtle-set y11Parents
        set GCSE-score sumAttainment / length y11Parents
        if(GCSE-score > 100) [ set GCSE-score 100 ]
      ]
    ]
  ]

  ;GCSE-score is the mean of the last Parent-Memory years, just like max-distances
  ask schools
  [
    set GCSE-scores lput GCSE-score GCSE-scores ;add the GCSE-scores to the list of GCSE-scores
    if(length GCSE-scores > Parent-Memory) [ set GCSE-scores remove-item 0 GCSE-scores ]  ;only keep track of last *Memory* years distances
  ]

  ask schools [ set GCSE-score mean GCSE-scores ]

end

to check-success

  ask parents with [child-age = 10 ]
  [
    let thisRank 0
    while[thisRank < Number-of-Ranks]
    [
      if(allocated-school = item thisRank rankings) [ set success-by-rank thisRank + 1 ]
      set thisRank thisRank + 1
    ]

    if(success-by-rank != -1) [ set success-rank1 0 ]
    if(success-by-rank = 1) [ set success-rank1 1 ]

    ifelse(aspiration < [GCSE-score] of allocated-school)
    [ set success-by-aspiration  1 ]
    [ set success-by-aspiration 0 ]
  ]

end

to set-child-attainment [ yParents ]

  if(not empty? yParents)
  [
    ask turtle-set yParents
    [
      if(School-Value-Added = true) [ set child-attainment child-attainment * (1 + [value-added] of myself) ]
      set child-attainment (child-attainment * (1 - School-Peer-Effect - Parent-Effect)) + ([mean-attainment] of myself * School-Peer-Effect) + (aspiration * Parent-Effect)

      if(child-attainment > 100) [ set child-attainment 100 ]
    ]
  ]

end

to set-patch-attributes ;for ALL patches

  ask patches
  [
    set location-value 0
    set p-aspiration 0
    set p-attainment 0
    set p-hhold-income 0
  ]

  ask patches
  [
;    ifelse(School-Quality-Effect > 0)
;    [
;      let nearestSchools sort-by [ [?1 ?2] -> [distance myself] of ?1 < [distance myself] of ?2 ] schools
;      let schoolValue [GCSE-score] of first nearestSchools
;
;      let dist distance first nearestSchools
;
;      ifelse(dist < (schoolRadii * 2))
;      [ set schoolValue (1 - ( dist / (schoolRadii * 2))^ 0.5) * schoolValue ]
;      [ set schoolValue 0 ]
;
;      ifelse(any? parents-on neighbors)
;      [ set location-value ((mean [aspiration] of parents-on neighbors) * (1 - School-Quality-Effect)) + (schoolValue * School-Quality-Effect) ]
;      [ set location-value 0 + (schoolValue * School-Quality-Effect) ]
;    ]

;    [
      if(any? parents-on neighbors) [ set location-value mean [hhold-income] of parents-on neighbors ]
;    ]
  ]

  ask patches with [location-value = 0]
  [
    print "zero location-value on neighbours"
    set location-value mean [location-value] of neighbors
  ]

  ask patches with [any? parents-here ]
  [
    set p-aspiration mean [aspiration] of parents-here        ;use mean to prevent run time error when checking patches without parents below
    set p-attainment mean [child-attainment] of parents-here  ;use mean to prevent run time error when checking patches without parents below
    set p-hhold-income mean [hhold-income] of parents-here
  ]

  ask patches with [not any? parents-here] [ set p-aspiration mean [p-aspiration] of neighbors ]
  ask patches with [not any? parents-here] [ set p-attainment mean [p-attainment] of neighbors ]
  ask patches with [not any? parents-here] [ set p-hhold-income mean [p-hhold-income] of neighbors ]

end

to set-location-value  ;for a SINGLE patch

;  ifelse(School-Quality-Effect > 0)
;  [
;    let nearestSchools sort-by [[distance myself] of ?1 < [distance myself] of ?2] schools
;    let schoolValue [GCSE-score] of first nearestSchools
;
;    let dist distance first nearestSchools
;
;    ifelse(dist < (schoolRadii * 2))
;    [ set schoolValue (1 - ( dist / (schoolRadii * 2))^ 0.5) * schoolValue ]
;    [ set schoolValue 0 ]
;
;    ifelse(any? parents-on neighbors)
;    [ set location-value ((mean [aspiration] of parents-on neighbors) * (1 - School-Quality-Effect)) + (schoolValue * School-Quality-Effect) ]
;    [ set location-value 0 + (schoolValue * School-Quality-Effect) ]
;  ]
;
;  [
    ifelse(any? parents-on neighbors)
    [ set location-value mean [hhold-income] of parents-on neighbors ]
    [ set location-value mean [location-value] of neighbors ]
;  ]

end

;;----------------------------------
;;DISPLAY
;;----------------------------------
to update-Colours

  let bestSchool max-one-of schools [GCSE-score]
  let worstSchool min-one-of schools [GCSE-score]
  let max-hhold-income 1
  let min-hhold-income 1000000
  ask parents
  [
    if ([hhold-income] of self < min-hhold-income) [set min-hhold-income [hhold-income] of self]
    if ([hhold-income] of self > max-hhold-income) [set max-hhold-income [hhold-income] of self]
  ]

  ;show parents with specified colours
  ifelse(Patch-Value = false)
  [
    ask patches [ set pcolor black ]

    ask parents
    [
      set hidden? false

      if(Parent-Colours = "satisfaction")
      [
        ifelse(child-age < 10 or allocated-school = 0)
        [ set color grey ]

        [
          if(Success-Type = "ranking")
          [
            let dcolor Number-of-Ranks / 3 ;set top third of ranks green, second third yellow, bottom third orange, unranked red
            if(Number-of-Ranks < 3) [ set dcolor Number-of-Ranks ]

            let thisRank 0

            ;if the allocated-school was ranked
            ifelse(member? allocated-school rankings)
            [
              while[thisRank < length rankings]
              [
                if(item thisRank rankings = allocated-school)
                [
                  ifelse(thisRank < dcolor) [ set color green ]
                  [
                    ifelse(thisRank < dcolor * 2) [ set color yellow ]
                    [ set color orange ]
                  ]
                ]
                set thisRank thisRank + 1
              ]
            ]

            ;if the allocated-school was not ranked set color red
            [ set color red ]
          ]

          if(Success-Type = "aspiration")
          [
            ifelse(aspiration < [GCSE-score] of allocated-school)
            [ set color green ]
            [ set color red ]
          ]

          if(Success-Type = "attainment")
          [
            ifelse(child-attainment >= aspiration)
            [ set color green ]
            [ set color red ]
          ]

        ]
      ]



      if(Parent-Colours = "school")
      [
        ifelse(child-age < 10 or allocated-school = 0)
        [ set color grey ]
        [ set color [id] of allocated-school * 10 + 15 ]
      ]

      if(Parent-Colours = "aspiration")
      [
        ifelse(aspiration = 100)
        [ set color 19.9 ]
        [ set color (aspiration / 10) + 10 ]
      ]

      if(Parent-Colours = "household income")
      [
;        let norm-hhold-income (hhold-income - min-hhold-income) / (max-hhold-income - min-hhold-income)
;        set color (norm-hhold-income * 5) + 15
;        ;set color (norm-hhold-income * 10) + 10
        set color (hhold-income / 20000) + 10
        if (color > 19.9) [set color 19.9]
      ]

      if(Parent-Colours = "attainment")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [
          ifelse(child-attainment = 100)
          [ set color 29.9 ]
          [ set color (child-attainment / 10) + 20 ]
        ]
      ]

      if(Parent-Colours = "attainment-change")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [
          let diffc child-attainment - aspiration

          ifelse(diffc < 0)
          [
            ifelse(diffc = -100)
            [ set color 22 ]
            [ set color 27 - (diffc / -20) ]
          ]

          [
            ifelse(diffc = 100)
            [ set color 62 ]
            [ set color 67 - (diffc / 20) ]
          ]

        ]
      ]

      if(Parent-Colours = "moved")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [
          ifelse(have-moved = true)
          [ set color green ]
          [ set color red ]
        ]
      ]

      if(Parent-Colours = "best school allocation")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [
          ifelse(member? bestSchool rankings)
          ;ifelse(item 0 rankings = bestSchool)
          [
            ifelse(allocated-school = bestSchool)
            [ set color green ]
            [ set color red ]
          ]
          [ set color grey]
        ]
      ]

      if(Parent-Colours = "worst school allocation")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [
          ifelse(member? worstSchool rankings)
          ;ifelse(item 0 rankings = worstSchool)
          [
            ifelse(allocated-school = worstSchool)
            [ set color green ]
            [ set color red ]
          ]
          [ set color grey]
        ]
      ]


      if(Parent-Colours = "strategy")
      [
        ifelse(allocated-school = 0)
        [ set color grey ]
        [ set color ((strategy * 10 + 4)) ]
      ]

      if(Parent-Colours = "age")
      [
        if(child-age = 9) [ set color 19 ]
        if(child-age = 10) [ set color 18 ]
        if(child-age = 11) [ set color 17 ]
        if(child-age = 12) [ set color 16 ]
        if(child-age = 13) [ set color 15 ]
        if(child-age = 14) [ set color 14 ]
        if(child-age = 15) [ set color 13 ]
        if(child-age = 16) [ set color 12 ]
      ]

      if(Parent-Colours = "allocated-distance")
      [
        set color (allocated-distance / 8) + 122
      ]

      if(Parent-Colours = "none")
      [
        set color black
      ]
    ]


    if(Show-Unallocated = false)
    [
      ask parents with [allocated-school = 0] [ set hidden? true ]
    ]


    if(ChildAge = "<9") [ ask parents with [child-age > 8] [ set color grey ] ]
    if(ChildAge = "9") [ ask parents with [child-age != 9] [ set color grey ] ]
    if(ChildAge = "10") [ ask parents with [child-age != 10] [ set color grey ] ]
    if(ChildAge = "11") [ ask parents with [child-age != 11] [ set color grey ] ]
    if(ChildAge = "12") [ ask parents with [child-age != 12] [ set color grey ] ]
    if(ChildAge = "13") [ ask parents with [child-age != 13] [ set color grey ] ]
    if(ChildAge = "14") [ ask parents with [child-age != 14] [ set color grey ] ]
    if(ChildAge = "15") [ ask parents with [child-age != 15] [ set color grey ] ]
    if(ChildAge = "16") [ ask parents with [child-age != 16] [ set color grey ] ]
    if(ChildAge = ">16") [ ask parents with [child-age < 17] [ set color grey ] ]
    if(ChildAge = "SchoolAge") [ ask parents with [child-age < 10 or child-age > 16 ] [ set color grey ] ]

    if(DistanceClass = "0-10") [ ask parents with [allocated-distance > 10] [ set color grey ] ]
    if(DistanceClass = "10-20") [ ask parents with [allocated-distance <= 10 or allocated-distance > 20] [ set color grey ] ]
    if(DistanceClass = "20-30") [ ask parents with [allocated-distance <= 20 or allocated-distance > 30] [ set color grey ] ]
    if(DistanceClass = "30-40") [ ask parents with [allocated-distance <= 30 or allocated-distance > 40] [ set color grey ] ]
    if(DistanceClass = "40-50") [ ask parents with [allocated-distance <= 40 or allocated-distance > 50] [ set color grey ] ]
    if(DistanceClass = "50-60") [ ask parents with [allocated-distance <= 50 or allocated-distance > 60] [ set color grey ] ]
    if(DistanceClass = ">60") [ ask parents with [allocated-distance <= 60 ] [ set color grey ] ]

    if(AspirationClass = "0-10") [ ask parents with [aspiration > 10] [ set color grey ] ]
    if(AspirationClass = "10-20") [ ask parents with [aspiration <= 10 or aspiration > 20] [ set color grey ] ]
    if(AspirationClass = "20-30") [ ask parents with [aspiration <= 20 or aspiration > 30] [ set color grey ] ]
    if(AspirationClass = "30-40") [ ask parents with [aspiration <= 30 or aspiration > 40] [ set color grey ] ]
    if(AspirationClass = "40-50") [ ask parents with [aspiration <= 40 or aspiration > 50] [ set color grey ] ]
    if(AspirationClass = "50-60") [ ask parents with [aspiration <= 50 or aspiration > 60] [ set color grey ] ]
    if(AspirationClass = "60-70") [ ask parents with [aspiration <= 60 or aspiration > 70] [ set color grey ] ]
    if(AspirationClass = "70-80") [ ask parents with [aspiration <= 70 or aspiration > 80] [ set color grey ] ]
    if(AspirationClass = "80-90") [ ask parents with [aspiration <= 80 or aspiration > 90] [ set color grey ] ]
    if(AspirationClass = "90-100") [ ask parents with [aspiration <= 90 or aspiration > 100] [ set color grey ] ]

  ]



  ;else don't show parents and show patch value
  [
;    ask parents [ set hidden? true ]
;
;    ifelse(Parent-Colours = "aspiration")
;    [
;      ask patches
;      [
;        ifelse(p-aspiration = 100)
;        [ set pcolor 19.9 ]
;        [ set pcolor (p-aspiration / 10) + 10 ]
;      ]
;    ]
;
;    [
;      ask patches
;      [
;        ifelse(house-price = -1)
;        [ set pcolor black ]
;        [ set pcolor (house-price / 10) + 10 ]
;      ]
;    ]
  ]



  ;always show schools
  let max-ratio max [app-ratio] of schools
  let max-value-added max [value-added] of schools
  let min-value-added min [value-added] of schools

  ask schools
  [
    if(School-Colours = "id")
    [
      set color (id * 10) + 15  ;first school will be red
    ]


    if(School-Colours = "GCSE")
    [
      ifelse(GCSE-score = 100)
      [ set color 49.9 ]
      [ set color (GCSE-score / 10) + 40 ]
    ]

    if(School-Colours = "app-ratio")
    [
      if(max-ratio != 0)
      [
        let scaled-ratio app-ratio / max-ratio
        ifelse(scaled-ratio = 1)
        [ set color 59.9 ]
        [ set color (scaled-ratio * 10) + 50 ]
      ]
    ]


    if(School-Colours = "value-added")
    [
      ifelse(value-added < 0)
      [
        let scaled-va value-added / (min-value-added * 2)
        ifelse(scaled-va = 0.5)
        [ set color 80.5 ]      ;don't set to 80 b/c unable to see icon on the grid
        [ set color 85 - (scaled-va * 10) ]
      ]
      [
        let scaled-va value-added / (max-value-added * 2)
        ifelse(scaled-va = 0.5)
        [ set color 89.9 ]
        [ set color (scaled-va * 10) + 85 ]
      ]
    ]

    if(Parent-Colours = "best school allocation")
    [
      ifelse(id != [id] of bestSchool)
      [ set color grey ]
      [ set color yellow ]
    ]

    if(Parent-Colours = "worst school allocation")
    [
      ifelse(id != [id] of worstSchool)
      [ set color grey ]
      [ set color yellow ]
    ]

  ]


  if(Single-School)
  [
    ask parents
    [
      ifelse(allocated-school = 0)
      [set color grey]
      [ if([id] of allocated-school != Shown-School) [ set color grey ] ]
    ]

    ask schools with [id != Shown-School ] [ set color grey ]
  ]

end

;;----------------------------------
;;PLOTTING
;;----------------------------------

to plotting

;  do-line-plots
  do-bar-plots
  do-scatter-plots

end

to do-bar-plots


  set-current-plot "Application-Ratio"
  clear-plot
  set-current-plot-pen "s"
  set-plot-pen-color black

  ifelse(Ordered-Plots = false)
  [ ask-concurrent schools [ plotxy id app-ratio ] ]
  [
    let tempSchools sort-by [ [?1 ?2] -> [app-ratio] of ?1 > [app-ratio] of ?2 ] schools
    foreach tempSchools [ [?1] -> plot [app-ratio] of ?1 ]
  ]




  set-current-plot "GCSE-scores"
  clear-plot
  set-current-plot-pen "s"

  ifelse(Ordered-Plots = false)
  [ ask-concurrent schools [ plotxy id GCSE-score ]  ]
  [
    let tempSchools sort-by [ [?1 ?2] -> [GCSE-score] of ?1 > [GCSE-score] of ?2 ] schools
    foreach tempSchools [ [?1] -> plot [GCSE-score] of ?1 ]
  ]

;  set-current-plot "Max-Distance"
;  clear-plot
;  set-current-plot-pen "s"
;
;  ifelse(Ordered-Plots = false)
;  [ ask-concurrent schools [ plotxy id max-distance ] ]
;  [
;    let tempSchools sort-by [ [max-distance] of ?1 > [max-distance] of ?2 ] schools
;    foreach tempSchools [ plot [max-distance] of ? ]
;  ]
;
 ;set-current-plot "BestSchool-ranks"
 ;clear-plot
 ;set-current-plot-pen "default"

 ;ask max-one-of schools [GCSE-score]
 ;[
 ;  let sumRanks 0
 ;  let thisRank 0
 ;  let ranks 0

 ;  while[thisRank < Number-of-Ranks]
 ;  [
 ;    set ranks 0

 ;    foreach allocated
 ;    [
 ;      if([item thisRank rankings] of ? = self)
 ;      [
 ;        set ranks ranks + 1
 ;        set sumRanks sumRanks + 1
 ;        ;ask ? [show rankings]
 ;      ]
 ;    ]

 ;    ;show ranks
 ;    ;show sumRanks
 ;    plotxy thisRank ranks
 ;    set thisRank thisRank + 1
 ;  ]

 ;  ;show length allocated
 ;  ;plot unranked
 ;  plotxy thisRank (length allocated) - sumRanks
 ;]



 ;set-current-plot "WorstSchool-ranks"
 ;clear-plot
 ;set-current-plot-pen "default"

 ;ask min-one-of schools [GCSE-score]
 ;[
 ;  let sumRanks 0
 ;  let thisRank 0

 ;  while[thisRank < Number-of-Ranks]
 ;  [
 ;    let ranks 0

 ;    foreach allocated
 ;    [
 ;      if([item thisRank rankings] of ? = self)
 ;      [
 ;        set ranks ranks + 1
 ;        set sumRanks sumRanks + 1
 ;        ;ask ? [show rankings]
 ;      ]
 ;    ]

 ;    plotxy thisRank ranks
 ;    set thisRank thisRank + 1
 ;  ]

 ;  ;plot unranked
 ;  plotxy thisRank length allocated - sumRanks
 ;]


  ;set-current-plot "aspiration-distribution"
  ;clear-plot
  ;set-current-plot-pen "default"
  ;histogram [aspiration] of parents with [child-age >= 10 and child-age <= 15 ]

end



to do-scatter-plots


  set-current-plot "MaxDistance-GCSE"
  clear-plot
  set-current-plot-pen "max"
  ask schools
  [
    ;set-plot-pen-color (id * 10) + 15
    set-plot-pen-color black
    plotxy GCSE-score max-distance
  ]

   set-current-plot "MaxDistance-App"
  clear-plot
  set-current-plot-pen "default"
  ask schools
  [
    ;set-plot-pen-color (id * 10) + 15
    set-plot-pen-color black
    plotxy app-ratio max-distance
  ]

  ;set-current-plot "MeanDistance-GCSE"
  ;clear-plot
  ;set-current-plot-pen "default"
  ;ask schools
  ;[
  ;  set-plot-pen-color (id * 10) + 15
  ;  plotxy GCSE-score mean-distance
  ;]

  ;set-current-plot "MeanDistance-App"
  ;clear-plot
  ;set-current-plot-pen "default"
  ;ask schools
  ;[
  ;  set-plot-pen-color (id * 10) + 15
  ;  plotxy app-ratio mean-distance
  ;]

  set-current-plot "AppRatio-GCSE"
  clear-plot
  set-current-plot-pen "default"
  ask schools
  [
    ;set-plot-pen-color (id * 10) + 15
    set-plot-pen-color black
    plotxy GCSE-score app-ratio
  ]

  if(School-Value-Added)
  [
    set-current-plot "AppRatio-ValueAdded"
    clear-plot
    set-current-plot-pen "default"
    ask schools
    [
      ;set-plot-pen-color (id * 10) + 15
      set-plot-pen-color black
      plotxy app-ratio value-added
    ]
  ]

end
;;----------------------------------
;;END PLOTTING
;;----------------------------------

;;----------------------------------
;;STATS
;;----------------------------------
to setup-R

  print "setting up R"

  r:clear
  r:eval("library(spdep)")

  r:eval("x <- c()")
  r:eval("y <- c()")

  ask patches with [within-lbbd = true]
  [
    let px [pxcor] of self
    let py [pycor] of self

    r:put "px" px
    r:put "py" py
    r:eval("y <- c(y,py)")

    r:eval("x <- c(x,px)")
    r:eval("y <- c(y,py)")
  ]
  r:eval("xy <- expand.grid(x, y)")

  r:eval("grid.nb <- tri2nb(xy)")

  r:eval("grid.listw <- nb2listw(grid.nb, style='C')")

  set asp-moran-i 0
  set asp-moran-i-p 0
  set att-moran-i 0
  set att-moran-i-p 0
  set inc-moran-i 0
  set inc-moran-i-p 0

  set GA-m 0
  set GA-r 0
  set GA-p 0

  set GMx-m 0
  set GMx-r 0
  set GMx-p 0

  set GMn-m 0
  set GMn-r 0
  set GMn-p 0

  set AMx-m 0
  set AMx-r 0
  set AMx-p 0

  set AMn-m 0
  set AMn-r 0
  set AMn-p 0

end
;
;
to calc-Moran

;17Jul12
;need to use patch attribute values for spatial autocorrelation (b/c using cell2nb in R to deal with toroid)
  ifelse(calc-Moran? and ticks > 0) ;ticks >= 20 to speed execution time for analysis
  [
    print "calc Moran's I (R)"

    ;aspiration
    r:put "p.aspiration" map [ ?1 -> [p-aspiration] of ?1 ] sort patches with [within-lbbd = true]

    r:eval("asp.i <- moran.test(p.aspiration, grid.listw)")

    set asp-moran-i r:get("asp.i$estimate[1]")
    set asp-moran-i-p r:get("asp.i$p.value")

    print word "Moran's i (aspiration): " asp-moran-i
    print word "Moran's i, p-value: " asp-moran-i-p

    ;attainment
    r:put "p.attainment" map [ ?1 -> [p-attainment] of ?1 ] sort patches with [within-lbbd = true]

    r:eval("att.i <- moran.test(p.attainment, grid.listw)")

    set att-moran-i r:get("att.i$estimate[1]")
    set att-moran-i-p r:get("att.i$p.value")

    print word "Moran's i (attainment): " att-moran-i
    print word "Moran's i, p-value: " att-moran-i-p

    ;income
    r:put "p.hholdinc" map [ ?1 -> [p-hhold-income] of ?1 ] sort patches with [within-lbbd = true]

    r:eval("inc.i <- moran.test(p.hholdinc, grid.listw)")

    set inc-moran-i r:get("inc.i$estimate[1]")
    set inc-moran-i-p r:get("inc.i$p.value")

    print word "Moran's i (hhold-income): " inc-moran-i
    print word "Moran's i, p-value: " inc-moran-i-p
  ]

  [
    ;create dummy values
    set asp-moran-i -99
    set asp-moran-i-p -99

    set att-moran-i -99
    set att-moran-i-p -99

    set inc-moran-i -99
    set inc-moran-i-p -99
  ]

end
;
;
;
;
;
;
;
;
to calc-relationships

print "calc relationships (R)"

r:put "sGCSEs" map [ ?1 -> [GCSE-score] of ?1 ] sort schools
r:put "sAppRatios" map [ ?1 -> [app-ratio] of ?1 ] sort schools
r:put "sMaxDists" map [ ?1 -> [max-distance] of ?1 ] sort schools
r:put "sMeanDists" map [ ?1 -> [mean-distance] of ?1 ] sort schools

r:eval("dat <- cbind(sGCSEs, sAppRatios, sMaxDists, sMeanDists)")
r:eval("dat <- as.data.frame(dat)")

r:eval("GCSEvsAppRatioLM <- lm(sGCSEs ~ sAppRatios, data = dat)")
r:eval("GCSEvsMaxDistLM <- lm(sGCSEs ~ sMaxDists, data = dat)")
r:eval("GCSEvsMeanDistLM <- lm(sGCSEs ~ sMeanDists, data = dat)")
r:eval("AppRatiovsMaxDistLM <- lm(sAppRatios ~ sMaxDists, data = dat)")
r:eval("AppRatiovsMeanDistLM <- lm(sAppRatios ~ sMeanDists, data = dat)")

set GA-m r:get("summary(GCSEvsAppRatioLM)$coefficients[2]")
set GA-r r:get("summary(GCSEvsAppRatioLM)$r.squared")
set GA-p r:get("anova(GCSEvsAppRatioLM)$'Pr(>F)'[1]")

set GMx-m r:get("summary(GCSEvsMaxDistLM)$coefficients[2]")
set GMx-r r:get("summary(GCSEvsMaxDistLM)$r.squared")
set GMx-p r:get("anova(GCSEvsMaxDistLM)$'Pr(>F)'[1]")

set GMn-m r:get("summary(GCSEvsMeanDistLM)$coefficients[2]")
set GMn-r r:get("summary(GCSEvsMeanDistLM)$r.squared")
set GMn-p r:get("anova(GCSEvsMeanDistLM)$'Pr(>F)'[1]")

set AMx-m r:get("summary(AppRatiovsMaxDistLM)$coefficients[2]")
set AMx-r r:get("summary(AppRatiovsMaxDistLM)$r.squared")
set AMx-p r:get("anova(AppRatiovsMaxDistLM)$'Pr(>F)'[1]")

set AMn-m r:get("summary(AppRatiovsMeanDistLM)$coefficients[2]")
set AMn-r r:get("summary(AppRatiovsMeanDistLM)$r.squared")
set AMn-p r:get("anova(AppRatiovsMeanDistLM)$'Pr(>F)'[1]")

end

;;----------------------------------
;;DATA EXPORT
;;----------------------------------
to ExportPlots

  let prefix "plots"
  let suffix ".csv"
  let index next-index prefix suffix
  let filename (word prefix index suffix)

  export-all-plots filename

end

to ExportView

  let prefix "interface"
  let suffix ".png"
  let index next-index prefix suffix
  let filename (word prefix index suffix)

  export-view filename

end

;from TurtleZero?
to-report next-index [ prefix suffix ]
  let index 0
  let filename (word prefix index suffix )
  while [ file-exists? filename ]
  [
    set index index + 1
    set filename (word prefix index suffix )
  ]
  report index
end




to ExportSummaryData

  show "Exporting summary data"

  ;file-open "Schools_SummaryData.csv"

  let dummy "Schools_SummaryData_Exp"
  let suffix ".csv"
  let filename (word dummy exp-index suffix)
  file-open filename

  ask schools
  [

    ;summarise y7 parents
    let max-distance_y7 max-one-of turtle-set y7Parents [distance myself]
    ifelse(max-distance_y7 = nobody)
    [ set max-distance_y7 -1 ]
    [ set max-distance_y7 distance max-distance_y7 ]


    let mean-distance_y7 0
    let mean-aspiration_y7 0
    let mean-attainment_y7 0
    let parents_moved_y7 0
    let parents_strategy1_y7 0
    let parents_strategy2_y7 0
    let parents_strategy3_y7 0
    let parents_strategy4_y7 0
    let parents_strategy5_y7 0
    let parents_strategy6_y7 0
    let parents_strategy7_y7 0
    let parents_strategy8_y7 0
    let success-aspiration 0

    foreach y7Parents
    [ [?1] ->
      set mean-distance_y7 mean-distance_y7 + distance ?1
      set mean-aspiration_y7 mean-aspiration_y7 + [aspiration] of ?1
      set mean-attainment_y7 mean-attainment_y7 + [child-attainment] of ?1
      if([have-moved] of ?1 = true) [ set parents_moved_y7 parents_moved_y7 + 1 ]

      if([strategy] of ?1 = 1) [ set  parents_strategy1_y7  parents_strategy1_y7 + 1 ]
      if([strategy] of ?1 = 2) [ set  parents_strategy2_y7  parents_strategy2_y7 + 1 ]
      if([strategy] of ?1 = 3) [ set  parents_strategy3_y7  parents_strategy3_y7 + 1 ]
      if([strategy] of ?1 = 4) [ set  parents_strategy4_y7  parents_strategy4_y7 + 1 ]
      if([strategy] of ?1 = 5) [ set  parents_strategy5_y7  parents_strategy5_y7 + 1 ]
      if([strategy] of ?1 = 6) [ set  parents_strategy6_y7  parents_strategy6_y7 + 1 ]
      if([strategy] of ?1 = 7) [ set  parents_strategy7_y7  parents_strategy7_y7 + 1 ]
      if([strategy] of ?1 = 8) [ set  parents_strategy8_y7  parents_strategy8_y7 + 1 ]

      if(GCSE-score < [aspiration] of ?1) [ set success-aspiration success-aspiration + 1 ]
    ]


    ifelse(length y7Parents > 0)
    [
      set mean-distance_y7 mean-distance_y7 / length y7Parents
      set mean-aspiration_y7 mean-aspiration_y7 / length y7Parents
      set mean-attainment_y7 mean-attainment_y7 / length y7Parents

    ]
    [
      set mean-distance_y7 -1
      set mean-aspiration_y7 -1
      set mean-attainment_y7 -1
    ]



    ;summarise y11 parents
    let max-distance_y11 max-one-of turtle-set y11Parents [distance myself]
    ifelse(max-distance_y11 = nobody)
    [ set max-distance_y11 -1 ]
    [ set max-distance_y11 distance max-distance_y11 ]


    let mean-distance_y11 0
    let mean-aspiration_y11 0
    let mean-attainment_y11 0
    let success-attainment 0


    foreach y11Parents
    [ [?1] ->
      set mean-distance_y11 mean-distance_y11 + distance ?1
      set mean-aspiration_y11 mean-aspiration_y11 + [aspiration] of ?1
      set mean-attainment_y11 mean-attainment_y11 + [child-attainment] of ?1
      if([child-attainment] of ?1 > [aspiration] of ?1) [ set success-attainment success-attainment + 1 ]
    ]

    ifelse(length y11Parents > 0)
    [
      set mean-distance_y11 mean-distance_y11 / length y11Parents
      set mean-aspiration_y11 mean-aspiration_y11 / length y11Parents
      set mean-attainment_y11 mean-attainment_y11 / length y11Parents
    ]

    [
      set mean-distance_y11 -1
      set mean-aspiration_y11 -1
      set mean-attainment_y11 -1
    ]


    let mean-attainment_change_y11 0
    let count-attainment_change+_y11 0
    let count-attainment_change-_y11 0

    ask turtle-set y11Parents
    [
      let my-attainment-change child-attainment - aspiration
      set mean-attainment_change_y11 mean-attainment_change_y11 + my-attainment-change

      ifelse(my-attainment-change > 0)
      [ set count-attainment_change+_y11 count-attainment_change+_y11 + 1 ]
      [ set count-attainment_change-_y11 count-attainment_change-_y11 + 1 ]
    ]

    ifelse(length y11Parents > 0)
    [ set mean-attainment_change_y11 mean-attainment_change_y11 / length y11Parents ]
    [ set mean-attainment_change_y11 -1 ]


    let unallocated []
    let all-applicants-list sort all-applicants
    foreach all-applicants-list
    [ [?1] ->
      if(not member? ?1 allocated) [set unallocated lput ?1 unallocated]
    ]
    let max-distance_unallocated max-one-of turtle-set unallocated [distance myself]
    ifelse(max-distance_unallocated = nobody)
    [ set max-distance_unallocated -1 ]
    [ set max-distance_unallocated distance max-distance_unallocated ]


    let mean-aspiration_unallocated 0
    let mean-distance_unallocated 0

    foreach unallocated
    [ [?1] ->
      set mean-aspiration_unallocated mean-aspiration_unallocated + [aspiration] of ?1
      set mean-distance_unallocated mean-distance_unallocated + distance ?1
    ]

    ifelse(length unallocated > 0)
    [
      set mean-aspiration_unallocated mean-aspiration_unallocated / length unallocated
      set mean-distance_unallocated mean-distance_unallocated / length unallocated
    ]
    [
      set mean-aspiration_unallocated -1
      set mean-distance_unallocated -1
    ]



    let myRanks [ 0 0 0 0 0 0 ]
    ask turtle-set allocated
    [

      let thisRank 0

      if(length rankings > 1)
      [
        while[thisRank < Number-of-Ranks]
        [
          if(item thisRank rankings = myself) [ set myRanks replace-item thisRank myRanks (item thisRank myRanks + 1) ]
          set thisRank thisRank + 1
        ]
      ]
    ]

    let parents_rank1_y7 item 0 myRanks
    let parents_rank2_y7 item 1 myRanks
    let parents_rank3_y7 item 2 myRanks
    let parents_rank4_y7 item 3 myRanks
    let parents_rank5_y7 item 4 myRanks
    let parents_rank6_y7 item 5 myRanks

    let parents_unranked_y7 0
    ask turtle-set allocated [ if(not member? myself rankings) [ set parents_unranked_y7 parents_unranked_y7 + 1 ] ]


    let parents_avoiding_y7 0
    ask parents with [child-age = 11]
    [
      if(member? myself avoided) [ set parents_avoiding_y7 parents_avoiding_y7 + 1 ]
    ]




    file-type ticks
    file-type ","
    file-type id
    file-type ","
    file-type school-type
    file-type ","
    file-type xcor
    file-type ","
    file-type ycor
    file-type ","
    file-type all-pupils
    file-type ","
    file-type precision value-added 2
    file-type ","
    file-type precision GCSE-score 2
    file-type ","
    file-type precision app-ratio 2
    file-type ","
    file-type precision max-distance_y7 2
    file-type ","
    file-type precision max-distance_y11 2
    file-type ","
    file-type precision max-distance_unallocated 2
    file-type ","
    file-type precision mean-distance_y7 2
    file-type ","
    file-type precision mean-distance_y11 2
    file-type ","
    file-type precision mean-distance_unallocated 2
    file-type ","
    file-type precision mean-aspiration_y7 2
    file-type ","
    file-type precision mean-aspiration_y11 2
    file-type ","
    file-type precision mean-aspiration_unallocated 2
    file-type ","
    file-type precision mean-attainment_y7 2
    file-type ","
    file-type precision mean-attainment_y11 2
    file-type ","
    file-type precision parents_rank1_y7 2
    file-type ","
    file-type precision parents_rank2_y7 2
    file-type ","
    file-type precision parents_rank3_y7 2
    file-type ","
    file-type precision parents_rank4_y7 2
    file-type ","
    file-type precision parents_rank5_y7 2
    file-type ","
    file-type precision parents_rank6_y7 2
    file-type ","
    file-type precision parents_unranked_y7 2
    file-type ","
    file-type precision success-aspiration 2
    file-type ","
    file-type precision success-attainment 2
    file-type ","
    file-type precision parents_avoiding_y7 2
    file-type ","
    file-type precision parents_moved_y7 2
    file-type ","
    file-type precision mean-attainment_change_y11 2
    file-type ","
    file-type precision count-attainment_change+_y11 2
    file-type ","
    file-type precision count-attainment_change-_y11 2
    file-type ","
    file-type precision parents_strategy1_y7 2
    file-type ","
    file-type precision parents_strategy2_y7 2
    file-type ","
    file-type precision parents_strategy3_y7 2
    file-type ","
    file-type precision parents_strategy4_y7 2
    file-type ","
    file-type precision parents_strategy5_y7 2
    file-type ","
    file-type precision parents_strategy6_y7 2
    file-type ","
    file-type precision parents_strategy7_y7 2
    file-type ","
    file-print precision parents_strategy8_y7 2

  ]

    file-close



    ;file-open "Parents_Data.csv"

    set dummy "Parents_Data_Exp"
    set filename (word dummy exp-index suffix)
    file-open filename

    ask parents
    [
       file-type ticks
       file-type ","
       file-type who
       file-type ","
       file-type xcor
       file-type ","
       file-type ycor
       file-type ","
       file-type precision aspiration 2
       file-type ","
       file-type child-age
       file-type ","

       ifelse(allocated-school != 0)
       [
         file-type [id] of allocated-school
         file-type ","

         file-type precision allocated-distance 2 ;07Sept12 moved this line here from next ifelse to ensure distance is always output
         file-type ","

         ifelse(is-turtle? first rankings)
         [
           file-type [id] of first rankings
           file-type ","
         ]
         [
           file-type -1
           file-type ","
         ]

         file-type success-by-rank
         file-type ","

         file-type success-rank1
         file-type ","

         file-type precision child-attainment 2
         file-type ","

         file-type strategy
         file-type ","

         ifelse(have-moved)
         [
           file-type 1
           file-type ","
         ]
         [
           file-type 0
           file-type ","
         ]

         file-print precision hhold-income 2

       ]



       [
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-type -1
         file-type ","
         file-print -1
       ]
    ]

    file-close

    ;file-open "World_SummaryData.csv"

    set dummy "World_SummaryData_Exp"
    set filename (word dummy exp-index suffix)
    file-open filename

    file-type ticks
    file-type ","
    file-type mean [GCSE-score] of schools
    file-type ","
    file-type ((max [GCSE-score] of schools) - (min [GCSE-score] of schools))
    file-type ","
    file-type mean [app-ratio] of schools
    file-type ","
    file-type ((max [app-ratio] of schools) - (min [app-ratio] of schools))
    file-type ","
    file-type asp-moran-i
    file-type ","
    file-type asp-moran-i-p
    file-type ","
    file-type att-moran-i
    file-type ","
    file-type att-moran-i-p
    file-type ","
    file-type inc-moran-i
    file-type ","
    file-type inc-moran-i-p
    file-type ","
    file-type GA-m
    file-type ","
    file-type GA-r
    file-type ","
    file-type GA-p
    file-type ","
    file-type GMx-m
    file-type ","
    file-type GMx-r
    file-type ","
    file-type GMx-p
    file-type ","
    file-type GMn-m
    file-type ","
    file-type GMn-r
    file-type ","
    file-type GMn-p
    file-type ","
    file-type AMx-m
    file-type ","
    file-type AMx-r
    file-type ","
    file-type AMx-p
    file-type ","
    file-type AMn-m
    file-type ","
    file-type AMn-r
    file-type ","
    file-print AMn-p

    file-close

end


to ExportSummaryData_header

  show "Exporting summary data"

  let dummy "Schools_SummaryData_Exp"
  let suffix ".csv"
  let filename (word dummy exp-index suffix)
  file-open filename
  write-experiment-data
  file-print "Tick,School_id,school-type,school-x-cor,school-y-cor,all-pupils,Value-Added,GCSE-score,App-ratio,max-distance_y7,max-distance_y11,max-distance_unallocated,mean-distance_y7,mean-distance_y11,mean-distance_unallocated,mean-aspiration_y7,mean-aspiration_y11,mean-aspiration_unallocated,mean-attainment_y7,mean-attainment_y11,parents_rank1_y7,parents_rank2_y7,parents_rank3_y7,parents_rank4_y7,parents_rank5_y7,parents_rank6_y7,parents_unranked_y7,success-aspiration_y7,success-attainment_y11,parents_avoiding_y7,parents_moved_y7,mean-attainment_change_y11,count-attainment_change+_y11,count-attainment_change-_y11,parents_strategy1_y7,parents_strategy2_y7,parents_strategy3_y7,parents_strategy4_y7,parents_strategy5_y7,parents_strategy6_y7,parents_strategy7_y7,parents_strategy8_y7"
  file-close

  set dummy "Parents_Data_Exp"
  set filename (word dummy exp-index suffix)
  file-open filename
  file-print word "Date/time: " date-and-time
  file-print "Tick,parent,x-cor,y-cor,aspiration,child-age,allocated-school,allocated-distance,preferred-school,allocated-rank,success-rank1,child-attainment,strategy,have-moved,hhold-income"
  file-close

  set dummy "World_SummaryData_Exp"
  set filename (word dummy exp-index suffix)
  file-open filename
  write-experiment-data
  file-print "Tick,MeanGCSE,RangeGCSE,MeanAppRatio,RangeAppRatio,asp-moran-i,asp-moran-i-p,pv-moran-i,pv-moran-i-p,GA-m,GA-r,GA-p,GMx-m,GMx-r,GMx-p,GMn-m,GMn-r,GMn-p,AMx-m,AMx-r,AMx-p,AMn-m,AMn-r,AMn-p"
  file-close

end



to write-experiment-data

  ;FILE MUST ALREADY BE OPEN!
  file-print word "Date/time: " date-and-time
  file-print word "seed," seed
  ;file-print word "Families," Families
  file-print word "Parent-Memory," Parent-Memory
  file-print word "SchoolSize," SchoolSize
  file-print word "Number-of-Schools," 10
  file-print word "Departure-Probability," 1
  file-print word "Number-of-Ranks," Number-of-Ranks
  ;file-print word "Random-Schools," Random-Schools
  ;file-print word "Min-School-Spacing," Min-School-Spacing
  file-print word "Initial-School-GCSE-Distribution," Initial-School-GCSE-Distribution
  file-print word "School-Value-Added," School-Value-Added
  ;file-print word "Parent-Aspiration-Distribution," Parent-Aspiration-Distribution
  ;file-print word "Aspiration-Mean," Aspiration-Mean
  file-print word "School-Value-Added-Distribution," School-Value-Added-Distribution
  ;file-print word "Location-Rules," Location-Rules
  file-print word "Move-Closest," Move-Closest
  file-print word "Avoid-Schools," Avoid-Schools
  file-print word "Avoided-Threshold," Avoided-Threshold
  file-print word "School-Peer-Effect," School-Peer-Effect
  ;file-print word "School-Quality-Effect," School-Quality-Effect
  file-print word "Parent-Effect," Parent-Effect
  ;file-print word "Attainment=Aspiration?," attainment=aspiration?

end


to ExportWorldData

  show "Exporting world data"

  let dummy "World_Exp"
  let filename (word dummy exp-index "_Tick" ticks ".csv")
  export-world filename

  ;let cmd (word "C:\\Program Files\\WinRAR\\WinRAR.exe\" A -dr -ep -ibck " directory "\\" "World_tick" ticks ".rar " directory "\\" filename)
  ;show cmd
  ;show shell:exec cmd

end


to ExportWorldData_initial

  show "Exporting world data"
  let filename (word "World_Exp" exp-index "_Tick_initial.csv")
  export-world filename

  ;let cmd (word "C:\\Program Files\\WinRAR\\WinRAR.exe\" A -dr -ep -ibck " directory "\\" "World_tick_initial.rar " directory "\\" filename)
  ;show cmd
  ;show shell:exec cmd

end




to export-worldview

  show "Exporting world view"
  let filename (word "WorldView_Exp" exp-index "_Tick" ticks ".png")
  export-view filename

end


;;----------------------------------
;;END DATA EXPORT
;;----------------------------------
@#$#@#$#@
GRAPHICS-WINDOW
210
10
951
752
-1
-1
5.3504
1
10
1
1
1
0
0
0
1
-68
68
-68
68
0
0
1
ticks
30.0

BUTTON
18
18
91
51
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
18
113
116
146
run-length
run-length
0
200
100.0
1
1
NIL
HORIZONTAL

CHOOSER
18
161
157
206
Initial-School-GCSE-Distribution
Initial-School-GCSE-Distribution
"uniform" "normal" "negative-exponential"
0

CHOOSER
17
221
157
266
School-Value-Added-Distribution
School-Value-Added-Distribution
"uniform" "normal"
1

SWITCH
17
284
159
317
School-Value-Added
School-Value-Added
0
1
-1000

BUTTON
115
18
178
51
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
339
189
372
Avoided-Threshold
Avoided-Threshold
0
1
0.75
0.05
1
NIL
HORIZONTAL

SWITCH
16
387
179
420
Avoid-Schools
Avoid-Schools
0
1
-1000

SLIDER
16
433
188
466
Number-of-Ranks
Number-of-Ranks
2
6
6.0
1
1
NIL
HORIZONTAL

SWITCH
16
481
158
514
Move-Closest
Move-Closest
1
1
-1000

SLIDER
13
530
186
563
Price-Income-Ratio
Price-Income-Ratio
4
9
7.0
0.5
1
NIL
HORIZONTAL

SLIDER
13
632
185
665
Parent-Memory
Parent-Memory
1
5
5.0
1
1
NIL
HORIZONTAL

SLIDER
13
681
185
714
School-Peer-Effect
School-Peer-Effect
0
0.5
0.25
0.05
1
NIL
HORIZONTAL

SLIDER
14
729
186
762
Parent-Effect
Parent-Effect
0
0.5
0.25
0.05
1
NIL
HORIZONTAL

SWITCH
17
787
148
820
Patch-Value
Patch-Value
1
1
-1000

CHOOSER
398
767
590
812
Parent-Colours
Parent-Colours
"satisfaction" "school" "aspiration" "attainment" "attainment-change" "moved" "best school allocation" "worst school allocation" "strategy" "age" "allocated-distance" "household income" "none"
0

CHOOSER
398
823
536
868
Success-Type
Success-Type
"ranking" "aspiration" "attainment"
0

SWITCH
401
881
570
914
Show-Unallocated
Show-Unallocated
0
1
-1000

CHOOSER
249
820
387
865
ChildAge
ChildAge
"All" "SchoolAge" ">16" "<9" "9" "10" "11" "12" "13" "14" "15" "16"
11

CHOOSER
249
872
387
917
DistanceClass
DistanceClass
"All" "0-10" "10-20" "20-30" "30-40" "40-50" "50-60" ">60"
0

CHOOSER
249
925
387
970
AspirationClass
AspirationClass
"All" "0-10" "10-20" "20-30" "30-40" "40-50" "50-60" "60-70" "70-80" "80-90" "90-100"
0

CHOOSER
248
980
386
1025
School-Colours
School-Colours
"id" "GCSE" "app-ratio" "value-added"
1

SWITCH
249
1032
391
1065
Single-School
Single-School
1
1
-1000

SLIDER
250
1076
422
1109
Shown-School
Shown-School
0
10
10.0
1
1
NIL
HORIZONTAL

SWITCH
250
1123
396
1156
Ordered-Plots
Ordered-Plots
1
1
-1000

PLOT
977
14
1176
164
MaxDistance-GCSE
GCSE-score
max-distance
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"max" 1.0 2 -2674135 true "" ""
"mean" 1.0 2 -16777216 true "" ""

PLOT
977
177
1177
327
AppRatio-GCSE
GCSE-score
app-ratio
0.0
100.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
978
338
1178
488
GCSE-scores
NIL
NIL
0.0
10.0
25.0
100.0
true
false
"" ""
PENS
"s" 1.0 1 -16777216 true "" ""

PLOT
1193
14
1393
164
MaxDistance-App
AppRatio
MaxDist
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1193
177
1393
327
AppRatio-ValueAdded
AppRatio
ValueAdded
0.0
10.0
-0.1
0.1
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""

PLOT
1195
338
1395
488
Application-Ratio
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"s" 1.0 1 -16777216 true "" ""

MONITOR
978
517
1071
570
Mean GCSE
mean [GCSE-score] of schools
1
1
13

MONITOR
1087
516
1185
569
Range GCSE
(max [GCSE-score] of schools) - (min [GCSE-score] of schools)
1
1
13

MONITOR
1201
517
1299
570
Mean App-ratio
mean [app-ratio] of schools
1
1
13

MONITOR
1307
516
1420
569
Range App-ratio
(max [app-ratio] of schools) - (min [app-ratio] of schools)
1
1
13

BUTTON
252
769
376
802
Update Display
update-Colours
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
21
835
142
868
Move-Best
Move-Best
0
1
-1000

SWITCH
669
769
802
802
calc-Moran?
calc-Moran?
1
1
-1000

SWITCH
671
817
868
850
Export-Summary-Data
Export-Summary-Data
1
1
-1000

SWITCH
671
865
846
898
Export-World-Data
Export-World-Data
1
1
-1000

SWITCH
672
914
813
947
Export-Movie
Export-Movie
1
1
-1000

SLIDER
12
583
184
616
SchoolSize
SchoolSize
100
150
100.0
10
1
NIL
HORIZONTAL

SLIDER
17
67
189
100
seed
seed
0
1000
818.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
