rm -Rf ~/uploads/*
rm -Rf ~/compiled/*
rm -Rf ~/games/*
cp -R ~/aichallenge/ants/maps/* ~/maps/
mysql -u contest -p$1 -D aichallenge < ~/aichallenge/sql/0_schema.sql
mysql -u contest -p$1 -D aichallenge < ~/aichallenge/sql/1_data.sql
mysql -u contest -p$1 -D aichallenge < ~/aichallenge/sql/2_generate_matchup.sql
mysql -u contest -p$1 -D aichallenge < ~/aichallenge/sql/3_worker.sql
python ~/aichallenge/manager/add_maps_to_database.py
python ~/aichallenge/integration_testing/create_test_bot.py ErrorBot -c 1
python ~/aichallenge/integration_testing/create_test_bot.py TimeoutBot -c 1
python ~/aichallenge/integration_testing/create_test_bot.py InvalidBot -c 1
python ~/aichallenge/integration_testing/create_test_bot.py HunterBot -c 3
python ~/aichallenge/integration_testing/create_test_bot.py LeftyBot -c 3
python ~/aichallenge/integration_testing/create_test_bot.py GreedyBot -c 3
python ~/aichallenge/integration_testing/create_test_bot.py RandomBot -c 3
