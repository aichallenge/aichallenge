FROM ubuntu:18.04

ENV API_CREATE_KEY dummy
ENV API_HOSTNAME 127.0.0.1

RUN apt-get update && apt-get install -y \
  curl \
  python \
  wget

COPY entrypoint.sh /

RUN chmod +x /entrypoint.sh

# CMD "curl http://${API_HOSTNAME}/api_server_setup.php?api_create_key=${API_CREATE_KEY}|sh"
# ENTRYPOINT ["curl", "http://$API_HOSTNAME/api_server_setup.php?api_create_key=$API_CREATE_KEY", "|", "sh" ]

ENTRYPOINT ["/entrypoint.sh"]