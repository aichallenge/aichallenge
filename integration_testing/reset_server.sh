rm -Rf uploads/*
rm -Rf compiled/*
rm -Rf games/*
cp -R aichallenge/ants/maps/* maps/
mysql -u contest -p$1 -D aichallenge < sql\0_schema.sql
mysql -u contest -p$1 -D aichallenge < sql\1_data.sql
mysql -u contest -p$1 -D aichallenge < sql\2_generate_matchup.sql
mysql -u contest -p$1 -D aichallenge < sql\3_worker.sql
manager\add_maps_to_database.py
manager\create_test_bot.py ErrorBot -c 1
manager\create_test_bot.py TimeoutBot -c 1
manager\create_test_bot.py InvalidBot -c 1
manager\create_test_bot.py HunterBot -c 3
manager\create_test_bot.py LeftyBot -c 3
manager\create_test_bot.py GreedyBot -c 3
manager\create_test_bot.py RandomBot -c 3
