
for i in 0{1..9} {10..50};
do 
	for j in 0{1..9} {10..50};
	do
		if [ $i -ne $j ]; then
			sort "$i".txt > "$i".sorted
			sort "$j".txt > "$j".sorted
		
			x=`diff -u "$j".sorted "$i".sorted | grep '^-'|wc -l`
			y=5
			if [ $x -le $y ]; then 
				echo $i
				echo $j
				echo $x
				echo
			fi
		fi

	done
done
read -p "Press [Enter] key to start backup..."