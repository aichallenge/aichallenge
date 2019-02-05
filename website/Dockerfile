FROM php:7.2.1-apache

# RUN apt-get update && apt-get install -y libmemcached-dev zlib1g-dev \
#   && pecl install memcached-2.2.0 \
#   && docker-php-ext-enable memcached

RUN docker-php-ext-install mysqli

ENV DATABASE_USER = contest
ENV DATABASE_PASSWORD = contest
ENV DATABASE_NAME = aichallenge
ENV WEBSITE_HOSTNAME = localhost
ENV UPLOAD_DIR = uploads
ENV MAPS_DIR = maps
ENV REPLAY_DIR = replays
ENV LOG_DIR = logs
ENV API_URL = api_url

ADD "config/000-default.conf" "/etc/apache2/sites-enabled/000-default.conf"
RUN a2enmod rewrite
