
for p in 0{1..9} {10..50};
do
	sort Country_"$p".txt > "$p".sorted
done
echo "created files" 
for i in 0{1..9} {10..50};
do 
	for j in 0{1..9} {10..50};
	do
		if [ $i -ne $j ]; then		
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
	echo done with $i
done
echo cleaning up
for p in 0{1..9} {10..50};
do
	rm "$p".sorted
done
read -p "done"