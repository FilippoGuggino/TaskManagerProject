package taskorganizer.webservice.TaskOrganizerWebService;

import com.rabbitmq.client.*;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;
import java.net.InetAddress;

/**
 * This class is only used to initialize some functionalities that will be used
 * later on during the execution. One of them is to call the RabbitMQManager init procedure
 * that will create a new queue dedicated to this host.
 */
@WebListener
public class HostInitializer implements ServletContextListener {
    public void contextInitialized(ServletContextEvent event) {
        RabbitMQManager.initRabbitMQ();
    }

    public void contextDestroyed(ServletContextEvent event) {
        // Do your thing during webapp's shutdown.
    }
}
