@echo off
pushd .
cd /d %0\..\..
rmdir /s /q submissions
mkdir submissions
rmdir /s /q worker\submissions
mkdir worker\submissions
rmdir /s /q worker\games
mkdir worker\games
rmdir /s /q maps
xcopy ants\maps maps /e /i /y
rmdir /s /q website\ants
mkdir website\ants
mkdir website\ants\games
mysql -u aichallenge -p%1 -D aichallenge < sql\0_schema.sql
mysql -u aichallenge -p%1 -D aichallenge < sql\1_data.sql
mysql -u aichallenge -p%1 -D aichallenge < sql\2_generate_matchup.sql
mysql -u aichallenge -p%1 -D aichallenge < sql\3_worker.sql
manager\add_maps_to_database.py
manager\create_test_bot.py ErrorBot -c 1
manager\create_test_bot.py TimeoutBot -c 1
manager\create_test_bot.py InvalidBot -c 1
manager\create_test_bot.py HunterBot -c 3
manager\create_test_bot.py LeftyBot -c 3
manager\create_test_bot.py GreedyBot -c 3
manager\create_test_bot.py RandomBot -c 3
popd
