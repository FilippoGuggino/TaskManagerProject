package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.*;
import com.rabbitmq.client.*;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicReference;

public class RabbitMQManager {
    private static String QUEUE_NAME;
    private static String EXCHANGE_NAME = "topics_boards";

    private static Connection connection = null;
    // This is the channel used to automatically consume updates coming from the primary
    private static Channel channel;

    private static ExecutorService executor
            = Executors.newSingleThreadExecutor();

    static {
        System.out.println("inizializzo cose!!!");
        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("172.18.0.160");
        try {
            connection = factory.newConnection();
        } catch (TimeoutException | IOException e) {
            e.printStackTrace();
        }

        String ip = "";
        try {
            ip = InetAddress.getLocalHost().getHostAddress().toString();
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        QUEUE_NAME = "webserverqueue-" + ip;

        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            System.out.println("Ho ricevuto un messaggio da rabbitmq");
            OtpInputStream otpIn = new OtpInputStream(delivery.getBody());
            try {
                UserBoardConcurrentHashmap.updateClients(otpIn.read_any());
            } catch (OtpErlangDecodeException e) {
                e.printStackTrace();
            }
        };

        try{
            channel = connection.createChannel();
            System.out.println("Connected to RabbitMQ");
            // Queue will auto-delete if no consumers are attached to it
            channel.queueDeclare(QUEUE_NAME, false, false, true, null);
            channel.basicConsume(QUEUE_NAME, true, deliverCallback, consumerTag -> {});
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Create thread used to update the boards of connected users through AJAX
    // Create queue dedicated to this host inside RabbitMQ
    public static void initRabbitMQ() {
        //UpdaterThread userUpdater = new UpdaterThread("host-" + ip);


//        Test connection between web-server and erlang-server
//        MessageManager man = new MessageManager();
//        try {
//            man.test();
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
    }


    /*
    * DeliverCallback deliverCallback = (consumerTag, delivery) -> {
            String message = new String(delivery.getBody(), "UTF-8");
            OtpInputStream mess = new OtpInputStream(delivery.getBody());
            OtpNode webserver;
            OtpMbox mbox;
            try {
                OtpErlangPid pid = mess.read_pid();
                System.out.println(" [x] Received '" + pid.toString() + "'");

                webserver = new OtpNode("webservice@172.18.0.2", "test");
                mbox = webserver.createMbox("webserver");
                // TODO i don't think this is needed
                mbox.registerName("webserver");
                OtpErlangObject[] msg = new OtpErlangObject[2];
                msg[0] = new OtpErlangAtom("election_vote");
                msg[1] = mbox.self();
                OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);
                System.out.println("tuple: " + formatted_msg.toString());
                mbox.send(pid, formatted_msg);
            } catch (OtpErlangDecodeException e) {
                e.printStackTrace();
            }
        };*/

    public synchronized static void createBinding(String boardTitle) {
        try (Channel channel = connection.createChannel();){
            channel.queueBind(QUEUE_NAME, EXCHANGE_NAME, boardTitle);
        } catch (IOException | TimeoutException e) {
            e.printStackTrace();
        }
    }

    public synchronized static void removeBinding(String boardTitle) {
        try (Channel channel = connection.createChannel();){
            channel.queueUnbind(QUEUE_NAME, EXCHANGE_NAME, boardTitle);
        } catch (IOException | TimeoutException e) {
            e.printStackTrace();
        }
    }

    /*
     * Fetch primary PID from rabbitmq
     * */
    public synchronized static OtpErlangPid fetchPrimary() {



//        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
//            System.out.println("Ho ricevuto un messaggio da rabbitmq");
//            OtpInputStream mess = new OtpInputStream(delivery.getBody());
//            try {
//                pid.set(mess.read_pid());
//            } catch (OtpErlangDecodeException e) {
//                e.printStackTrace();
//            }
//            System.out.println("Primary pid: " + pid.toString());
//            channel.basicNack(delivery.getEnvelope().getDeliveryTag(), false, true);
//        };

//        try(Channel channel = connection.createChannel()){
//            channel.basicConsume("primary_queue", false, "one_time_tag", new DefaultConsumer(channel) {
//                @Override
//                public void handleDelivery(String consumerTag,
//                                           Envelope envelope,
//                                           AMQP.BasicProperties properties,
//                                           byte[] body)
//                        throws IOException
//                {
//                    System.out.println("Ho ricevuto un messaggio da rabbitmq");
//                    OtpInputStream mess = new OtpInputStream(body);
//                    try {
//                        pid.set(mess.read_pid());
//                    } catch (OtpErlangDecodeException e) {
//                        e.printStackTrace();
//                    }
//                    System.out.println("Primary pid: " + pid.toString());
//                    channel.basicNack(envelope.getDeliveryTag(), false, true);
//                    channel.basicCancel(consumerTag);
//                }
//            });
//        } catch (IOException | TimeoutException e) {
//            e.printStackTrace();
//        }


            OtpErlangPid pid = null;
            try (Channel channel = connection.createChannel();) {
                GetResponse response = null;
                while (response == null) {
                    // get message from "primary_queue" without sending ann ACK back
                    // This will result with the message not being cancelled from the queue -> other web-server
                    // may need the same message

                    response = channel.basicGet("primary_queue", false);
                }
                // Tells Rabbitmq to requeue the same message, many webserver may need info on the primary
                channel.basicNack(response.getEnvelope().getDeliveryTag(), false, true);

                OtpInputStream mess = new OtpInputStream(response.getBody());
                pid = mess.read_pid();
                System.out.println("Primary pid: " + pid.toString());
            } catch (IOException | OtpErlangDecodeException | TimeoutException e) {
                e.printStackTrace();
            }
            return pid;
    }
}
