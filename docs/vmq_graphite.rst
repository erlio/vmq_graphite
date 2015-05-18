.. _vmq_graphite:

vmq_graphite
============

The graphite plugin reports the broker metrics at a fixed interval (defined in Milliseconds) to a graphite server. The necessary configuration is done inside the ``vernemq.conf``.

.. code-block:: ini

    graphite.interval = 15000
    graphite.host = carbon.hostedgraphite.com
    graphite.port = 2003
    graphite.api_key = YOUR-GRAPHITE-API-KEY

You can further tune the connection to the Graphite server:

.. code-block:: ini

    # set the connect timeout (defaults to 5000 ms)
    graphite.connect_timeout = 10000

    # set a graphite prefix (defaults to 'vernemq')
    graphite.prefix = myprefix

.. tip::

    The above configuration parameters can be changed at runtime using the ``vmq-admin`` script.

