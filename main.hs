import System.Random (randomRIO) -- Эта функция используется для генерации случайных чисел в заданном диапазоне,
--необходимых для перемешивания колоды.

import Control.Monad (forM) -- forM – это функция высшего порядка, которая позволяет применять функцию к каждому
--элементу списка и возвращать список результатов. Она используется для генерации случайной
--перестановки элементов списка (перемешивания колоды).

-- Типы данных
data Card = Card Suit Rank deriving (Show, Eq) -- Определяет тип данных Card (карта),который состоит из двух
--полей: Suit (масть) и Rank (ранг).

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq, Enum) -- Определяет перечислимый тип данных
--Suit (масть) с четырьмя вариантами: Hearts, Diamonds, Clubs, Spades.

data Rank = Two | Three | Four | Five |
  Six | Seven | Eight | Nine | Ten | 
  Jack | Queen | King | Ace deriving (Show, Eq, Enum) -- Определяет перечислимый тип данных Rank (ранг) карты.

-- Значение карты 
cardValue :: Card -> Int -- Функция, которая принимает карту (Card) и возвращает ее числовое значение.
cardValue (Card _ r) = -- Обращается к полю Rank карты (r).
  case r of -- case-выражение для определения значения карты в зависимости от ранга. Туз (Ace) имеет значение 11,
--остальные карты — значение, соответствующее их порядковому номеру (с учетом корректировки fromEnum r + 1).
    Ace -> 11
    _ -> val r
  where
    val Ten = 10
    val Jack = 10
    val Queen = 10
    val King = 10
    val r = fromEnum r + 2 -- Прибавляем 2, так как Two имеет значение 0 в Enum


-- Колода карт 
createDeck :: [Card] -- Создает стандартную колоду из 52 карт.
createDeck = [Card s r | s <- [Hearts, Diamonds, Clubs, Spades],
  r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]] -- Использует list comprehension
--(список-понимание) для генерации всех возможных комбинаций мастей и рангов.

-- Перемешивание колоды
shuffleDeck :: [a] -> IO [a] -- Перемешивает колоду карт. [a] означает, что функция может работать с любым типом данных,
--а IO [a] говорит о том, что функция имеет побочный эффект (генерация случайных чисел) и возвращает результат в виде монады IO.
shuffleDeck deck = do -- Использует do-нотацию для работы с монадой IO.
  let n = length deck -- Определяет длину колоды.
  shuffled <- forM [1..n] $ \_ -> randomRIO (0, n-1)  -- Генерирует список случайных индексов (от 0 до n-1) с помощью forM,
--каждый индекс соответствует позиции карты в перемешанной колоде. _ означает, что значение индекса
--не используется в анонимной функции.
  return $ map (deck !!) shuffled -- Создает новую колоду, используя сгенерированные случайные индексы для
--перестановки карт. (deck !!) — это функция доступа к элементу списка по индексу.

-- Подсчет очков
calculateScore :: [Card] -> Int -- Подсчитывает сумму очков карт на руках.
calculateScore cards =
  let score = sum $ map cardValue cards -- Суммирует значения всех карт в руке.
      numAces = length $ filter (\(Card _ r) -> r == Ace) cards -- Подсчитывает количество тузов в руке.
  in if score > 21 && numAces > 0 then calculateScore (replaceAce cards) else score -- Если сумма очков больше 21 и есть
--хотя бы один туз, то вызывается функция replaceAce для замены туза на значение 1, иначе возвращается текущая сумма очков.

-- Замена туза
replaceAce :: [Card] -> [Card] -- Заменяет туза (Ace) на двойку (Two), если сумма очков превышает 21.
replaceAce cards = map (\(Card s r) -> if r == Ace then Card s (Two) else (Card s r)) cards

--  Игра игрока 
playerTurn :: [Card] -> [Card] -> IO ([Card], [Card]) -- Принимает руку игрока и колоду в качестве входных данных,
--и возвращает обновлённую руку игрока и оставшуюся колоду.
playerTurn hand deck = do
  putStrLn $ "Ваши карты: " ++ show hand ++ " (очки: " ++ show (calculateScore hand) ++ ")"
  if calculateScore hand > 21
    then return (hand, deck) -- Выходим из функции, если перебор
    else do
      putStrLn "Взять карту (h) или остановиться (s)?"
      choice <- getLine
      case choice of
        "h" -> do
          let (newCard:restDeck) = deck
          putStrLn $ "Вам выпала карта: " ++ show newCard
          playerTurn (newCard : hand) restDeck
        "s" -> return (hand, deck) -- Выходим из функции, если игрок остановился
        _ -> do
          putStrLn "Неверный ввод!"
          playerTurn hand deck

-- Игра дилера 
dealerTurn :: [Card] -> [Card] -> [Card] -> IO () -- Дилер берет карты до тех пор, пока сумма очков не достигнет 17 или больше.
dealerTurn dealerHand playerHand deck = do
  putStrLn $ "Карты дилера: " ++ show dealerHand ++ " (очки: " ++ show (calculateScore dealerHand) ++ ")"
  let playerScore = calculateScore playerHand
      dealerScore = calculateScore dealerHand

  if dealerScore > 21 then
    putStrLn "Дилер перебрал! Вы победили!"
  else if playerScore > 21 then
    putStrLn "Вы перебрали! Дилер победил!"
  else if dealerScore >= 17 then -- Дилер останавливается, если >= 17
    determineWinner playerScore dealerScore
  else do
    let (newCard:restDeck) = deck
    putStrLn $ "Дилер взял карту: " ++ show newCard
    dealerTurn (newCard : dealerHand) playerHand restDeck

determineWinner :: Int -> Int -> IO () -- Определяет победителя на основе очков игрока и дилера.
determineWinner playerScore dealerScore =
  if dealerScore > playerScore then
    putStrLn "Дилер победил!"
  else if playerScore > dealerScore then
    putStrLn "Вы победили!"
  else
    putStrLn "Ничья!"


-- Основная функция 
main :: IO ()
main = do
  deck <- shuffleDeck createDeck -- Перемешивает колоду.
  let (playerHand, restDeck) = Prelude.splitAt 2 deck -- Раздает первые две карты игроку.
      (dealerHand, finalDeck) = Prelude.splitAt 2 restDeck -- Раздает первые две карты дилеру.
  (newPlayerHand, updatedDeck) <- playerTurn playerHand finalDeck -- Вызывает функцию playerTurn для хода игрока.
  dealerTurn dealerHand newPlayerHand updatedDeck -- Вызывает функцию dealerTurn для хода дилера.