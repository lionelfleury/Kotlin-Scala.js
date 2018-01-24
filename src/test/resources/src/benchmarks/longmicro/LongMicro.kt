/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Benchmarks        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2016, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \                               **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package benchmarks.longmicro

import benchmarks.Benchmark
import benchmarks.collections.LinkedList
import benchmarks.longmicro.LongMicroDataSets.random32s
import benchmarks.longmicro.LongMicroDataSets.random53s
import benchmarks.longmicro.LongMicroDataSets.random64s
import benchmarks.longmicro.LongMicroDataSets.random8s
import benchmarks.longmicro.LongMicroDataSets.randomPow2s

fun main(args: Array<String>) {
    LongMicroAll.main(args)
}

object LongMicroAll : Benchmark() {
    private val allBenches: Array<LongMicro> = arrayOf(
            LongNop,
            LongXor,
            LongAdd,
            LongMul,
            LongDiv32_32,
            LongDiv32_8,
            LongDiv53_53,
            LongDiv53_8,
            LongDiv64_Pow2,
            LongDiv64_64,
            LongDiv64_8,
            LongToString32,
            LongToString53,
            LongToString64
    )

    override fun main(args: Array<String>) {
        for (i in 0 until allBenches.size) {
            val bench = allBenches[i]
            bench.main(args)
        }
    }

    override fun report(): String {
        val l = LinkedList<LongMicro>(allBenches)
        val reports = l.map { bench -> bench.prefix + ": " + bench.report() + " -- "}
        return reports.joinToString("")
    }

    override fun run(): Unit = throw Exception("Not implemented")
}

object LongMicroDataSets {

    val random64s = arrayOf<Long>(
            4713683788047544701L,
            -7684102831879253520L,
            -4708829119507791170L,
            -4311544113499576560L,
            8778661247579384360L,
            -8941202479484756382L,
            -719738821675955238L,
            8042618343890308875L,
            473682363136087275L,
            7389920342130397512L,
            -8166055962780398214L,
            7674130235834380448L,
            555629342811322371L,
            -5697555612306420391L,
            -7658431033881046540L,
            -7495608561042775764L,
            2311778142870538623L,
            -7212484014087185605L,
            -1055250280813380430L,
            -2884448576126335166L,
            3772692524156409284L,
            2381625196834501645L,
            984672066847250608L,
            8805009250641300777L,
            -9144537676097030649L,
            -2376127870830421026L,
            2388240501426009858L,
            470342326610764636L,
            3240195503172179670L,
            8489698232051387443L,
            2184618382235016524L,
            -6427612648511171832L,
            4504747627891707536L,
            1588765476882820741L,
            1003755627562828872L,
            3337371329248829585L,
            -6407404511079037976L,
            8267636061947815617L,
            -4939302742052264380L,
            3725077472853147149L,
            -1817868353991760794L,
            92055824954158534L,
            6048938955329428897L,
            -1020501032890479161L,
            -6366148055670494490L,
            8614069223376780266L,
            -3062503787839384747L,
            -5974996486387272514L,
            -8037620384147507690L,
            -4300594795679087754L,
            -879626572372968086L,
            3018418876209085731L,
            8757438573326954236L,
            5944035821653162789L,
            7242375099625516413L,
            6530545415228066015L,
            2368722352101818223L,
            4881361778636880041L,
            6673535797480746815L,
            -3872926174441090038L,
            8382482333642332313L,
            8331931893836028710L,
            7333783945492690680L,
            8211531622745524085L,
            -8396486254775727749L,
            -9203550717375560267L,
            -7392540492297740289L,
            -7482257570046862559L,
            8405964522267174249L,
            -3881597700969289245L,
            -4820439377443194045L,
            1105192502152939718L,
            806102173408148106L,
            6375470608314409519L,
            -5585801466867028539L,
            -6086279573162422707L,
            2794467363385383001L,
            -3126240726086853489L,
            3075798240890003227L,
            986144929013448790L,
            -7593686559272147417L,
            6482436625623110891L,
            -7779076815065122833L,
            -867423919379911501L,
            -8887274178300737130L,
            -5946400134637936735L,
            -4131674104277634946L,
            1643998385327230187L,
            1164785316725054751L,
            -8422417668093443072L,
            788345005062418202L,
            4212972240275343544L,
            2909792738287187597L,
            2036533835281458893L,
            -5527609710435076439L,
            -8866373377366046583L,
            -935285177949471360L,
            6221485651302319297L,
            -6302861776947763933L,
            6415709984925489153L
    )

    val random32s = arrayOf<Long>(
            1031522930L,
            1846696720L,
            -215379673L,
            -554040174L,
            -291462449L,
            -1207729799L,
            168713996L,
            1300434721L,
            -283596156L,
            1325674433L,
            316397733L,
            -480932415L,
            1911340691L,
            -1453926970L,
            1678523349L,
            1154973617L,
            161259328L,
            -409129959L,
            -292187349L,
            1699924747L,
            1493547511L,
            1454309911L,
            -705336217L,
            -2086892815L,
            651308395L,
            2093439286L,
            -1671236803L,
            -1540977402L,
            1284598786L,
            1964466592L,
            1606513752L,
            -1515802080L,
            550379631L,
            -978047995L,
            1158253729L,
            134686267L,
            713767072L,
            -439540563L,
            1585416384L,
            1359848368L,
            1322007259L,
            -765023284L,
            -378423070L,
            -2041104987L,
            -865384157L,
            1739939609L,
            -1969111876L,
            204025287L,
            1908504288L,
            -1578375007L,
            1361578960L,
            -479248074L,
            -1731439426L,
            1662412503L,
            1590771183L,
            2051670209L,
            -607791403L,
            834388464L,
            -2031560706L,
            957358163L,
            -1143867127L,
            1991148141L,
            1767743740L,
            -1931984468L,
            -541604429L,
            -990118664L,
            879215427L,
            1070085715L,
            1204920960L,
            1988224884L,
            2056863245L,
            -2117997366L,
            549128089L,
            2141170511L,
            1657105210L,
            -1730839293L,
            1845495044L,
            2113538329L,
            -625697457L,
            -77395949L,
            -344638903L,
            916250970L,
            -1587209791L,
            -1118937307L,
            1965368408L,
            -1875888481L,
            -1310002316L,
            424781232L,
            1563026857L,
            -2085438250L,
            1494518141L,
            1184588524L,
            312011941L,
            41781723L,
            -1777226193L,
            1018672769L,
            -1088938940L,
            -1065764014L,
            498580053L,
            -74734463L
    )

    val random53s = arrayOf<Long>(
            2816172990059844L,
            -2221416822750059L,
            1362799444965132L,
            1298912974311876L,
            1928984561937080L,
            -1885129686532067L,
            1049465334671045L,
            -3226497395582151L,
            557773066358386L,
            -987893620899422L,
            -1712794868502308L,
            -2308965823106607L,
            1311509261573742L,
            4166445084750561L,
            -261661129862907L,
            -1644700214959334L,
            -3605882452541879L,
            -2458358441137701L,
            -3079883240287894L,
            3889884951972938L,
            751723881570690L,
            2913643851832494L,
            -3081385313915721L,
            10148286976805L,
            -2069422543858408L,
            2180862834473956L,
            2434874786642981L,
            1217051911189256L,
            3216656552807385L,
            3663642207733885L,
            567980127414491L,
            231216833824579L,
            -3763891489302371L,
            497241975759603L,
            960094069800907L,
            -2017568579853290L,
            366407873096099L,
            -3074966346975109L,
            1315123042086760L,
            3981623756519297L,
            -628782464450464L,
            4461924351876025L,
            1294731307867739L,
            -2743066092563659L,
            -2522385831999241L,
            1386190466219574L,
            -3276583254953337L,
            3973817012709593L,
            -3216414470398831L,
            1586135105382346L,
            3488586776397717L,
            3841128427874795L,
            3029156840464878L,
            -2038863972814524L,
            -965898188866485L,
            -1230050650905474L,
            -715330239387816L,
            449009320980361L,
            3535715761563120L,
            3192492425190922L,
            2688550479493438L,
            3984116998431856L,
            1634351117836528L,
            -393788040899165L,
            4334831741362363L,
            1485569524920978L,
            1564836690730519L,
            448118794682044L,
            2739637079416142L,
            2453908380678496L,
            -1139005332163698L,
            -1889711648182838L,
            -174883938822930L,
            -3628648637962590L,
            1177021408174306L,
            2765949253294089L,
            -3943167071019561L,
            1927037960303299L,
            2880093257309529L,
            4440878661791625L,
            -4305184653349580L,
            2512786275869956L,
            -1160047958118450L,
            1596188710325479L,
            3220549893561851L,
            507758298866937L,
            3634349805172567L,
            -2585933022560299L,
            2918162363935539L,
            -2710938223327624L,
            528625767838713L,
            -4268043319277632L,
            1346640949928159L,
            -1070436378925761L,
            246132105217694L,
            -271726637081138L,
            -1261119525785899L,
            -2754651617161825L,
            -3032202487560291L,
            -839160326643332L
    )

    val random8s = arrayOf<Long>(-105L,
            49L,
            -88L,
            38L,
            -43L,
            12L,
            75L,
            22L,
            53L,
            4L,
            98L,
            -42L,
            -33L,
            26L,
            99L,
            -94L,
            -57L,
            -94L,
            -36L,
            -118L,
            86L,
            22L,
            -74L,
            102L,
            74L,
            38L,
            -67L,
            6L,
            -38L,
            18L,
            95L,
            -82L,
            28L,
            60L,
            -77L,
            -63L,
            -85L,
            -8L,
            -67L,
            -69L,
            -124L,
            -90L,
            -16L,
            100L,
            48L,
            3L,
            -13L,
            66L,
            40L,
            -69L,
            -89L,
            78L,
            116L,
            66L,
            105L,
            -21L,
            -108L,
            -29L,
            -108L,
            19L,
            -92L,
            24L,
            73L,
            -45L,
            -70L,
            28L,
            114L,
            -8L,
            -110L,
            -65L,
            3L,
            10L,
            109L,
            120L,
            -126L,
            121L,
            53L,
            -64L,
            -55L,
            22L,
            -27L,
            -99L,
            10L,
            -60L,
            39L,
            124L,
            20L,
            110L,
            62L,
            67L,
            -44L,
            59L,
            37L,
            -90L,
            29L,
            10L,
            -8L,
            -70L,
            -85L,
            -10L
    )

    val randomPow2s = arrayOf<Long>(
            68719476736L,
            288230376151711744L,
            2305843009213693952L,
            1099511627776L,
            512L,
            16384L,
            576460752303423488L,
            33554432L,
            8192L,
            131072L,
            70368744177664L,
            65536L,
            32768L,
            1099511627776L,
            4398046511104L,
            32768L,
            4611686018427387904L,
            9007199254740992L,
            144115188075855872L,
            144115188075855872L,
            33554432L,
            4096L,
            1L,
            4194304L,
            268435456L,
            1024L,
            536870912L,
            8589934592L,
            8L,
            512L,
            8796093022208L,
            536870912L,
            33554432L,
            17592186044416L,
            274877906944L,
            268435456L,
            256L,
            512L,
            9007199254740992L,
            4398046511104L,
            2147483648L,
            8796093022208L,
            8589934592L,
            9007199254740992L,
            144115188075855872L,
            js("Kotlin.Long.MIN_VALUE"),
            4194304L,
            4096L,
            js("Kotlin.Long.MIN_VALUE"),
            67108864L,
            4611686018427387904L,
            1L,
            549755813888L,
            18014398509481984L,
            4503599627370496L,
            4L,
            8796093022208L,
            134217728L,
            144115188075855872L,
            4L,
            64L,
            512L,
            140737488355328L,
            8388608L,
            1125899906842624L,
            4096L,
            2305843009213693952L,
            140737488355328L,
            72057594037927936L,
            2251799813685248L,
            562949953421312L,
            36028797018963968L,
            js("Kotlin.Long.MIN_VALUE"),
            4503599627370496L,
            4294967296L,
            16777216L,
            1L,
            32L,
            36028797018963968L,
            16384L,
            4294967296L,
            256L,
            8L,
            36028797018963968L,
            32L,
            67108864L,
            549755813888L,
            16L,
            1024L,
            274877906944L,
            33554432L,
            34359738368L,
            1L,
            33554432L,
            1152921504606846976L,
            2147483648L,
            32768L,
            137438953472L,
            17592186044416L,
            64
    )
}

/**
 * Long micro-benchmarks.
 */
abstract class LongMicro: Benchmark() {

    fun doRun(randomAs: Array<Long>, randomBs: Array<Long>): Long {
        val alen = randomAs.size
        val blen = randomBs.size
        var result = 0L
        var i = 0
        while (i != alen) {
            var j = 0
            while (j != blen) {
                val a = randomAs[i]
                val b = randomBs[j]
                result = result xor binaryOp(a, b)
                j += 1
            }
            i += 1
        }
        return result
    }

    abstract fun binaryOp(a: Long, b: Long): Long
}

object LongNop : LongMicro() {
    override val prefix = "LongNop"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != 0L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a
}

object LongXor : LongMicro() {
    override val prefix = "LongXor"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != 0L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a xor b
}

object LongAdd : LongMicro() {
    override val prefix = "LongAdd"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != -3199834553443988620L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a + b
}

object LongMul : LongMicro() {
    override val prefix = "LongMul"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != -4111379928290889828L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a * b
}

object LongDiv32_32 : LongMicro() {
    override val prefix = "LongDiv32_32"

    override fun run(): Unit {
        if (doRun(random32s, random32s) != 54L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv32_8 : LongMicro() {
    override val prefix = "LongDiv32_8"

    override fun run(): Unit {
        if (doRun(random32s, random8s) != 396463647L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv53_53 : LongMicro() {
    override val prefix = "LongDiv53_53"

    override fun run(): Unit {
        if (doRun(random53s, random53s) != 41L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv53_8 : LongMicro() {
    override val prefix = "LongDiv53_8"

    override fun run(): Unit {
        if (doRun(random53s, random8s) != 39173628864963L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv64_Pow2 : LongMicro() {
    override val prefix = "LongDiv64_Pow2"

    override fun run(): Unit {
        if (doRun(random64s, randomPow2s) != 475661794007097238L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv64_64 : LongMicro() {
    override val prefix = "LongDiv64_64"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != 64L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongDiv64_8 : LongMicro() {
    override val prefix = "LongDiv64_8"

    override fun run(): Unit {
        if (doRun(random64s, random8s) != 1415998624949685666L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a / b
}

object LongToString32 : LongMicro() {
    override val prefix = "LongToString32"

    override fun run(): Unit {
        if (doRun(random32s, random32s) != 0L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a.toString().length.toLong()
}


object LongToString53 : LongMicro() {
    override val prefix = "LongToString53"

    override fun run(): Unit {
        if (doRun(random53s, random53s) != 0L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a.toString().length.toLong()
}

object LongToString64 : LongMicro() {
    override val prefix = "LongToString64"

    override fun run(): Unit {
        if (doRun(random64s, random64s) != 0L)
            throw Exception("wrong result")
    }

    override fun binaryOp(a: Long, b: Long): Long = a.toString().length.toLong()
}
