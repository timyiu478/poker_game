ranks = [ "A", "K", "Q", "J", "O", "9", "8", "7", "6", "5", "4", "3", "2", "1" ]

suits = ["s", "h", "c", "d"]

findSuits(l, [], s) = []
findSuits(l, ranks, s) 
 | (elem ((head ranks) ++ s) l) = [ (head ranks) ++ s ] ++ findSuits(l, (tail ranks), s) 
 | otherwise = findSuits(l, (tail ranks), s)

findAllSuits(l, ranks, []) = []
findAllSuits(l, ranks, suits) = [ (findSuits(l, ranks, (head suits))) ] ++ findAllSuits(l, ranks, (tail suits))

findAllFlush(l) = [s | s <- findAllSuits(l,ranks,suits),length(s) > 4 ]

isFlush(l) = length (findAllFlush(l)) > 0

fiveSequence(ranks) = take 5 ranks

findSequence(l,[]) = []
findSequence(l, seq)
 | elem ((head seq) ++ "s") l = [ ((head seq) ++ "s") ] ++ findSequence(l, (tail seq))
 | elem ((head seq) ++ "h") l = [ ((head seq) ++ "h") ] ++ findSequence(l, (tail seq))
 | elem ((head seq) ++ "c") l = [ ((head seq) ++ "c") ] ++ findSequence(l, (tail seq))
 | elem ((head seq) ++ "d") l = [ ((head seq) ++ "d") ] ++ findSequence(l, (tail seq))
 | otherwise = findSequence(l, (tail seq))

findStraight(l, []) = []
findStraight(l, ranks) = [ (findSequence(l, fiveSequence(ranks))) ] ++ findStraight(l, (tail ranks))

isStraight(l) = elem 5 [length seq| seq <- findStraight(l, ranks)]

remove elements list = filter (\e -> (elem e (elements))==False) list

findSameRank([],rank) = []
findSameRank(l,rank) = filter (\e -> e == (rank ++ "s") || e == (rank ++ "h") || e == (rank ++ "c") || e == (rank ++ "d") ) l

findAllSameRanks(l,[]) = []
findAllSameRanks(l,ranks) = [ findSameRank(l, (head ranks)) ] ++ findAllSameRanks(l, (tail ranks))

isFourKind(l) = elem True [(length s) == 4| s <- findAllSameRanks(l,ranks)] && (length l) > 4

findSameNumRanks(l,n) = filter (\list -> (length list) >= n) (findAllSameRanks(l,ranks))

isFullHouse(l) = elem True [ length (findSameNumRanks((remove three l),2)) >=1 |three <- findSameNumRanks(l,3)]

isRoyalFlush(l) = elem True [isFlush(s) |s <- findStraight(l,ranks)]

isComb list = isStraight(list) || isFlush(list) || isFourKind(list) || isFullHouse(list) || isRoyalFlush(list)

verifyCatComb list = (length (filter (\e -> e == True) ([isStraight(list)] ++ [isFlush(list)] ++ [isFourKind(list)] ++ [isFullHouse(list)] ++ [isRoyalFlush(list)]))) > 1

findRoyalFlush(l) = [ (filter (\e -> ((length e) == 5)) (findAllSuits(s,ranks,suits) )) |s <- findStraight(l,ranks)]
 
findHighestRoyalFlush(l) = head (head([ s |s <- findRoyalFlush(l), length(s) == 1]))

findHighestFourKind(l) = (head (findSameNumRanks(l,4))) ++ [ (head (remove (head (findSameNumRanks(l,4)) ) l)) ]

findHighestFullHouse(l) = (take 3 (head (findSameNumRanks(l,3)))) ++ (head (head ([ (findSameNumRanks((remove three l),2)) |three <- findSameNumRanks(l,3)])))

findHighestFlush(l,ranks)
 | length ([ f | f<- findAllFlush(l), (elem (head (ranks)) ([ [(head (s))] | s<-f ]) ) ]) == 1 =  (take 5 (head ([ f | f<- findAllFlush(l), (elem (head (ranks)) ([ [(head (s))] | s<-f ]) ) ]) ))
 | length (tail ranks) > 0 = findHighestFlush(l, (tail ranks))
 | otherwise = (head (findAllFlush(l)))

findHighestStraight(l) = head ([ s | s <- findStraight(l,ranks), length(s) == 5 ])

highestComb list
 | (isRoyalFlush(list)) = findHighestRoyalFlush(list)
 | (isFourKind(list)) = findHighestFourKind(list)
 | (isFullHouse(list)) = findHighestFullHouse(list)
 | (isFlush(list)) = findHighestFlush(list,ranks)
 | (isStraight(list)) = findHighestStraight(list)
 | otherwise = []

genCards(rank) = [ ([rank] ++ (s)) | s <- suits]

-- (take 2 ( genCards( (head (remove [(head (head (head (findSameNumRanks(l,3)))))] (ranks)))) ) 

fillThreeSameRank l
 | (length (findSameNumRanks(l,3)) > 0) && ((length (l)) > 3) = (head (findSameNumRanks(l,3))) ++ [ (head (remove (head (findSameNumRanks(l,3))) l)) ]
 | (length (findSameNumRanks(l,3)) > 0) = (head (findSameNumRanks(l,3))) ++ (take 2 (genCards(head (head ([ r |r <- ranks, r /= [(head (head (head (findSameNumRanks(l,3)))))] ])))))

editFullHouse(l)
 | (length (findSameNumRanks(l,2)) > 1) = [ "-" ++ c | c <- (remove ((head (findSameNumRanks(l,2))) ++ (head (tail (findSameNumRanks(l,2))))) l )] ++ [(head ( [ "+" ++ c | c <- (remove (head (findSameNumRanks(l,2))) (genCards((head (head (head (findSameNumRanks(l,2)))))))) ] ) )]
 | (length (findSameNumRanks(l,3)) < 1) = ranks ++ ranks 
 | [ "-" ++ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (l)), (head (c)) /= (head (head (findMaxNumFourKind(l)))) ] == [] = [ "-" ++ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (findMaxNumFourKind(l)) ) ] ++ [ "-" ++ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (l)), (head (c)) /= (head (head (findMaxNumFourKind(l)))) ] ++ [ "+" ++ c | c <- (take 2 (genCards(head (head ([r | r<-ranks, r /= [ (head (head (findMaxNumFourKind(l)))) ] ])))))]
 | otherwise = [ "-" ++ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (findMaxNumFourKind(l)) ) ] ++ (tail ([ "-" ++ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (l)), (head (c)) /= (head (head (findMaxNumFourKind(l)))) ])) ++ [("+" ++ (head (remove [(firstSubCard l)] (genCards((head (firstSubCard l)))))))]

firstSubCard l = (head ([ c | c <- (remove (take 3 (findMaxNumFourKind(l))) (l)), (head (c)) /= (head (head (findMaxNumFourKind(l)))) ])) 



findMaxLengthList(l)
 | (length l) == 1 = (head l)
 | (length l) == 2 && (length (head l)) >= (length (head (tail l))) = (head l)
 | (length l) == 2 && (length (head l)) < (length (head (tail l))) = (head (tail l))
 | (length (head l)) >= (length (head (tail l))) = findMaxLengthList( ([(head l)] ++ (tail (tail l))) )
 | otherwise = findMaxLengthList((tail l))

findMinLenList(l)
 | (length l) == 1 = (head l)
 | (length l) == 2 && (length (head l)) <= (length (head (tail l))) = (head l)
 | (length l) == 2 && (length (head l)) > (length (head (tail l))) = (head (tail l))
 | (length (head l)) < (length (head (tail l))) = findMinLenList( ([(head l)] ++ (tail (tail l))) )
 | otherwise = findMinLenList((tail l))


findMaxNumSuit(l) = findMaxLengthList( findAllSuits(l,ranks,suits) )

editFlush(l) = [ "-" ++ c |c<-(remove (findMaxNumSuit(l)) l )] ++  (take (5 - (length (findMaxNumSuit(l)))) ([ "+" ++ r ++ (tail (head (findMaxNumSuit(l)))) | r <- ranks, (elem r (findMaxNumSuit(l))) == False ]))

findMaxNumStraight(l) = findMaxLengthList( findStraight(l,ranks) )

editStraight(l) = [ "-" ++ c |c<-(remove (findMaxNumStraight(l)) l )] ++ [ "+" ++ r ++ "s" | r <- fiveRanksFrom(ranks, (head ([ [head c] | c <- findMaxNumStraight l]))), (elem r ([ [head c] | c <- findMaxNumStraight l])) == False ]

fiveRanksFrom([],r) = []
fiveRanksFrom(ranks,r)
 | (r == "1" || r == "2" || r == "3" || r == "4") =  fiveRanksFrom(ranks,"5")
 | (length ranks) == 5 = ranks
 | (head ranks) /= r = fiveRanksFrom((tail ranks),r)
 | otherwise = take 5 (ranks)

findMaxNumFourKind(l) = findMaxLengthList( findAllSameRanks(l,ranks) )

editFourKind(l)
 | (elem True [(length s) == 4| s <- findAllSameRanks(l,ranks)]) = [ "+" ++ (head ([ r | r <- ranks, r /= (head ([ [head e] | e<- l  ])) ])) ++ "s" ]
 | [ "-" ++ c | c <- l, (elem c (findMaxNumFourKind(l)) ) == False] == [] = [ "+" ++ c |c <- genCards(head (head (findMaxNumFourKind(l)))), (elem c (findMaxNumFourKind(l)) ) == False] ++ ["+" ++ head [ r | r<- ranks, r /= [(head (head (findMaxNumFourKind(l))))] ] ++ "s"]
 | otherwise = (tail [ "-" ++ c | c <- l, (elem c (findMaxNumFourKind(l)) ) == False]) ++ [ "+" ++ c |c <- genCards(head (head (findMaxNumFourKind(l)))), (elem c (findMaxNumFourKind(l)) ) == False]

showComb l = ( [editFourKind(l)] ++ [editStraight(l)] ++ [editFlush(l)] ++ [editFullHouse(l)] )

editComb l
 |  l == [] = ["+1s","+2s","+3s","+4s","+5s"]
 | (highestComb l) /= [] = [ "-" ++ s  | s <- (remove (highestComb l) l)]
 | otherwise = findMinLenList( (showComb l) )