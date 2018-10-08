package boot;

import java.io.*;
import java.util.HashMap;

public class App {
    public static String getClojureName() { return "clojure"; }
    public static boolean isWindows() throws Exception {
        return (System.getProperty("os.name").toLowerCase().indexOf("win") >= 0);}

    public static HashMap<String, String>
        config() throws Exception {
        HashMap<String, String> ret = new HashMap<>();

        ret.putAll(System.getenv());
        ret.put("BOOT_COLOR", "true");

        return ret; }

    public static String
        config(String k) throws Exception {
        return config().get(k); }

    public static String
        config(String k, String dfl) throws Exception {
        String v = config(k);
        if (v != null) return v;
        else { System.setProperty(k, dfl); return dfl; }}

    public static File
        getBootDir() throws Exception {
        return bootdir(); }

    public static File
        bootdir() throws Exception {
        File   h = new File(System.getProperty("user.home"));
        String a = System.getProperty("BOOT_HOME");
        String b = System.getenv("BOOT_HOME");
        String c = new File(h, ".boot").getCanonicalPath();
        return new File((a != null) ? a : ((b != null) ? b : c)); }
}
