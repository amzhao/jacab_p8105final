Help\! I’ve Fallen and I Can’t Get Up: A Report
================
Team JACAB: Jai, Amelia, Carolina, Amanda, & Bing Bing
12/1/2019

\*Note: The title of this project is a line from a commercial that has
been
[memed](https://www.google.com/search?q=life+alert+meme&sxsrf=ACYBGNQqIvW74KMRbdauXsERsqWnXo-X_Q:1575226120265&source=lnms&tbm=isch&sa=X&ved=2ahUKEwiWjdvKjpXmAhVHq1kKHccFBD8Q_AUoAXoECAsQAw&biw=1130&bih=1257)
and is often the subject of jokes. It is in no way meant to trivialize
the emergencies experienced by 911 callers or the work of EMS
responders.

# Motivation

In New York City, emergencies abound. It’s hard to imagine a day in New
York without the sound of wailing sirens and flashing emergency vehicle
lights. New York City

Numerous studies have shown that socioeconomic status has a large impact
on health outcomes. Socioeconomic status is also very influential on
where a person lives, especially in New York City. Our group sought to
assess the

# Related Work

# Initial Questions

# Data

There were two data sources for this project:

1.  [NYC EMS Incident Dispatch
    Data](https://data.cityofnewyork.us/Public-Safety/EMS-Incident-Dispatch-Data/76xm-jjuj),
    owned by NYCOpenData and provided by the Fire Department of New York
    City.

2.  [2017 New York City Housing and Vacancy Survey
    Microdata](https://www.census.gov/data/datasets/2017/demo/nychvs/microdata.html),
    provided by the United States Census Bureau.

# Exploratory analysis

# Additional analysis

# Discussion

One of the biggest limitations of our study is that the data did not
contain any variable indicating the type of response unit sent. The FDNY
is in command of many different response units, which all correspond to
different types of emergencies.

Additionally, the data is only recorded once the incident is created in
the FDNY system. If you’ve ever called 911 (hopefully you haven’t had
to), you’ll know that the first person you talk to is not a police
officer, emergency medical technician, or fire fighter. It’s a 911 Call
Taker, as shown by this schematic provided by [NYC
Analytics](http://home2.nyc.gov/html/911reporting/html/anatomy/call.shtml).
These call takers ask tons of questions, the most notable being “What’s
your emergency” to assess the appropriate emergency response team for
the situation. A police officer may not be as helpful as a firefighter
for a building fire.

After the dispatcher determines the best response team for the
emergency, they will either put you directly in contact with someone
from say, the fire department or police, or they will pass the
information along to them while they stay on the line with you. (In the
NYC Analytics schematic, this is shown under FDNY EMS VESTA in Orange as
911 Call Conference). Then, either the caller or call taker may have to
relay the pertinent information to the response team dispatcher.

Our data is collected only after the first 911 call taker has passed the
call along to the FDNY EMS (Incident creation in the schematic). Thus,
it does not include the amount of time the caller spent explaining their
emergency to the dispatcher before being transferred to the FDNY. In
many cases, this can be a lengthy process, but from our data it is
impossible to tell. Additionally, it is unclear from the data whether,
from the NYC Analytics schematic, whether our incident creation is at
the general incident creation between “Call Taking” and “Skip to
Dispatch” in yellow at the top, or the “EMS Incident Creation” between
“911 Call Conference” and “Dispatch Queue” in orange at the bottom of
the schematic.

911 calls are emergencies, so time is crucial. The time before the
incident is created could influence not only the resulting morbidity or
mortality of those involved in the emergencies, it could also influence
the amount of time the FDNY spends on the phone before a unit is
dispatched. For example, if the 911 caller is in some way incapacitated,
the initial call taker may have difficulties assessing the emergency and
identifying the appropriate response team. The FDNY EMS dispatcher may
also then have difficulties assessing what type of unit to send, or even
where to send the unit.
