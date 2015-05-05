#Reg Ex
#	.at matches any three char string ending with at ex. "cat", "hat"
#	[hc]at = "hat" & "cat"
#	[^b]at = everything following .at except "bat"
#	[^hc]at = everything following .at except "cat" & "hat"
#	^[hc]at = matches "hat" and "cat" but only at the beginning of the string or line
#	[hc]at = matcges "hat" and "cat" but only at the end of a string or line
#	/[./] matches any single character surrounded by "[" and "]" since the brackets are escaped, ex: "[a]" or "[b]"
#	s.* matches any number of characters receded by s for example saw and seed
#
#	matching 
#		gray|grey
		