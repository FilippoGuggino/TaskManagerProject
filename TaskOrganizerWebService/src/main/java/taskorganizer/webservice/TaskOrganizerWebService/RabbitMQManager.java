package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.*;
import com.rabbitmq.client.*;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.concurrent.TimeoutException;

import static java.lang.System.exit;

public class RabbitMQManager {
    private static String QUEUE_NAME;
    private static String EXCHANGE_NAME = "topics_boards";

    private static Connection connection = null;
    private static Channel channel = null;

    static {
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("172.18.0.160");
        try {
            connection = factory.newConnection();
            channel = connection.createChannel();
        } catch (TimeoutException | IOException e) {
            e.printStackTrace();
        }
    }

    // Create thread used to update the boards of connected users through AJAX
    // Create queue dedicated to this host inside RabbitMQ
    public static void initRabbitMQ() {
        String ip = "";
        try {
            ip = InetAddress.getLocalHost().toString();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        QUEUE_NAME = "host-" + ip;
        //UpdaterThread userUpdater = new UpdaterThread("host-" + ip);

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            String message = new String(delivery.getBody(), "UTF-8");
            System.out.println(" [x] Received '" + message + "'");
        };

        try {
            System.out.println("Connected to RabbitMQ");
            channel.queueDeclare(QUEUE_NAME, false, false, false, null);
            channel.basicConsume(QUEUE_NAME, true, deliverCallback, consumerTag -> {
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public synchronized static void createBinding(String boardTitle) {
        try {
            channel.queueBind(QUEUE_NAME, EXCHANGE_NAME, boardTitle);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public synchronized static void removeBinding(String boardTitle) {
        try {
            channel.queueUnbind(QUEUE_NAME, EXCHANGE_NAME, boardTitle);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /*
     * Fetch primary PID from rabbitmq
     * */
    public synchronized static OtpErlangPid fetchPrimary() {
        OtpErlangPid pid = null;
        try {
            GetResponse response = null;
            while (response == null) {
                // get message from "primary_queue" without sending ann ACK back
                // This will result with the message not being cancelled from the queue -> other web-server
                // may need the same message
                response = channel.basicGet("primary_queue", false);
            }
            OtpInputStream mess = new OtpInputStream(response.getBody());
            pid = mess.read_pid();
            System.out.println("Primary pid: " + pid.toString());
        } catch (IOException | OtpErlangDecodeException e) {
            e.printStackTrace();
        }
        return pid;

    }
}
